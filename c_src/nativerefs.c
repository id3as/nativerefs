#include <erl_nif.h>

typedef struct _nif_globals {
  ErlNifEnv *global_env;
  ErlNifResourceType *ref_resource_type;
  ErlNifResourceType *ref_lock_resource_type;
  ERL_NIF_TERM undefined_atom;
} nif_globals;

typedef struct _waiter {
  ErlNifEnv *env;
  ErlNifPid waiter;
  ERL_NIF_TERM ref;
  struct _waiter *next;
} waiter;

typedef struct _ref {
  ERL_NIF_TERM term;
  ErlNifMutex* mutex;
  int locked;
  waiter *waiters;
} ref;

typedef struct _ref_lock {
  ref* ref;
} ref_lock;

static void awake_waiter(ErlNifEnv *callingEnv, nif_globals *globals, ref *ref) {
  if (ref->waiters) {
    waiter *wakeup = ref->waiters;
    ref->waiters = wakeup->next;
    enif_send(callingEnv, &wakeup->waiter, wakeup->env, wakeup->ref);
    enif_free_env(wakeup->env);
    free(wakeup);
  }
}

static ERL_NIF_TERM ref_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = (ref *) enif_alloc_resource(globals->ref_resource_type, sizeof(ref));
  r->term = enif_make_copy(globals->global_env, argv[0]);
  r->mutex = enif_mutex_create("native_ref_lock");
  r->locked = 0;
  r->waiters = NULL;
  ERL_NIF_TERM resource = enif_make_resource(env, r);
  enif_release_resource(r);
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), resource);
}

// Just read - no locking, you get whatever you get
static ERL_NIF_TERM ref_read(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 1) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_arity")); 
  }
  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = NULL;
  
  if(!enif_get_resource(env, argv[0], globals->ref_resource_type, (void**)&r)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg")); 
  }

  return enif_make_tuple2(env, enif_make_atom(env, "ok"), enif_make_copy(env, r->term));
}

// Read within a mutex, and set the locked flag.  No one else can now read until the lock is cleared
// through ref_write_with_lock.  If currently locked, returns 'busy'
static ERL_NIF_TERM ref_try_read_with_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 1) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_arity")); 
  }

  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = NULL;
  
  if(!enif_get_resource(env, argv[0], globals->ref_resource_type, (void**)&r)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg")); 
  }

  if(enif_mutex_trylock(r->mutex) == 0) {
    if (r->locked) {
      // Already claimed by someone else
      enif_mutex_unlock(r->mutex);
      return enif_make_atom(env, "busy");
    }

    // Do the work we need to do inside the mutex
    r->locked = 1;
    ERL_NIF_TERM copy = enif_make_copy(env, r->term);
    enif_keep_resource(r);
    enif_mutex_unlock(r->mutex);

    // And return the term and a ref to the lock
    ref_lock *l = (ref_lock *) enif_alloc_resource(globals->ref_lock_resource_type, sizeof(ref_lock));
    l->ref = r;

    ERL_NIF_TERM resource = enif_make_resource(env, l);
    enif_release_resource(l);

    return enif_make_tuple3(env, enif_make_atom(env, "ok"), resource, copy);
  } else {
    return enif_make_atom(env, "busy");
  }
}

// Read within a mutex, and set the locked flag.  No one else can now read until the lock is cleared
// through ref_write_with_lock.  If currently locked, blocks
static ERL_NIF_TERM ref_read_with_lock_(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 2) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_arity")); 
  }

  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = NULL;
  
  if(!enif_get_resource(env, argv[0], globals->ref_resource_type, (void**)&r)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg")); 
  }

  enif_mutex_lock(r->mutex);
  if (r->locked == 1) {
    if (!enif_is_identical(argv[1], globals->undefined_atom)) {
      // if locked, then we can't proceed.  We stash the caller in a list and return.
      // On unlock we pop the head off the list and send it a message and it calls back into here
      waiter *new_waiter = malloc(sizeof(*new_waiter));
      new_waiter->env = enif_alloc_env();
      new_waiter->next = NULL;
      enif_self(env, &new_waiter->waiter);
      new_waiter->ref = enif_make_copy(new_waiter->env, argv[1]);
      if (r->waiters == NULL) {
         r->waiters = new_waiter;
      }
      else {
           waiter *p = r->waiters;
           waiter *t = r->waiters;
           while (t != NULL) {
                 p = t;
                 t = t->next;
           }
           p->next = new_waiter;
      }
    }
    enif_mutex_unlock(r->mutex);
    return enif_make_atom(env, "busy");
  }
  else {
    r->locked = 1;
    ERL_NIF_TERM copy = enif_make_copy(env, r->term);
    enif_keep_resource(r);
    enif_mutex_unlock(r->mutex);

    ref_lock *l = (ref_lock *) enif_alloc_resource(globals->ref_lock_resource_type, sizeof(ref_lock));
    l->ref = r;

    ERL_NIF_TERM resource = enif_make_resource(env, l);
    enif_release_resource(l);

    return enif_make_tuple3(env, enif_make_atom(env, "ok"), resource, copy);
  }
}

// Writes and clears the lock
static ERL_NIF_TERM ref_write_with_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 2) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_arity")); 
  }

  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref_lock *l = NULL;

  if(!enif_get_resource(env, argv[0], globals->ref_lock_resource_type, (void**)&l)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg")); 
  }

  if(l->ref == NULL) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "already_used")); 
  }

  enif_mutex_lock(l->ref->mutex);

  l->ref->term = enif_make_copy(globals->global_env, argv[1]);
  l->ref->locked = 0;
  awake_waiter(env, globals, l->ref);
  enif_mutex_unlock(l->ref->mutex);

  enif_release_resource(l->ref);

  l->ref = NULL;
  
  return enif_make_atom(env, "ok");
}

// Locks and writes, blocks if already locked
static ERL_NIF_TERM ref_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 2) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_arity")); 
  }

  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = NULL;

  if(!enif_get_resource(env, argv[0], globals->ref_resource_type, (void**)&r)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg")); 
  }

  enif_mutex_lock(r->mutex);
  // We don't need to set the locked flag here, since we're only holding the resource for the duration of this nif call
  r->term = enif_make_copy(globals->global_env, argv[1]);
  enif_mutex_unlock(r->mutex);
  
  return enif_make_atom(env, "ok");
}

// Locks and writes, returns 'busy' if already locked
static ERL_NIF_TERM ref_try_write(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 2) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_arity")); 
  }

  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = NULL;

  if(!enif_get_resource(env, argv[0], globals->ref_resource_type, (void**)&r)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg")); 
  }

  if(enif_mutex_trylock(r->mutex) == 0) {
    if(r->locked) {
      enif_mutex_unlock(r->mutex);
      return enif_make_atom(env, "busy");
    }

    r->term = enif_make_copy(globals->global_env, argv[1]);
    enif_mutex_unlock(r->mutex);
    return enif_make_atom(env, "ok");
  } else {
    return enif_make_atom(env, "busy");
  }
}

static void ref_destroy(ErlNifEnv *env, void *obj)
{
  ref *r = (ref *) obj;
  enif_mutex_destroy(r->mutex);
}

static void ref_lock_destroy(ErlNifEnv *env, void *obj)
{
  ref_lock *l = (ref_lock *) obj;
  if (l->ref != NULL) {
    nif_globals *globals = (nif_globals *) enif_priv_data(env);
    // We have the ref locked, free it up
    enif_mutex_lock(l->ref->mutex);
    l->ref->locked = 0;
    awake_waiter(env, globals, l->ref);
    enif_mutex_unlock(l->ref->mutex);
    enif_release_resource(l->ref);
  }
  l->ref = NULL;
}

static int load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  nif_globals *globals = malloc(sizeof(*globals));
  *priv_data = globals;

  globals->ref_resource_type = enif_open_resource_type(env, NULL, "Ref", ref_destroy, ERL_NIF_RT_CREATE, NULL);
  globals->ref_lock_resource_type = enif_open_resource_type(env, NULL, "RefLock", ref_lock_destroy, ERL_NIF_RT_CREATE, NULL);
  globals->global_env = enif_alloc_env();
  globals->undefined_atom = enif_make_atom(globals->global_env, "undefined");

  return 0;
}

static ErlNifFunc nif_funcs[] =
  {
    {"ref_new", 1, ref_new},
    {"ref_read", 1, ref_read},
    {"ref_try_write", 2, ref_try_write},
    {"ref_try_read_with_lock", 1, ref_try_read_with_lock}, 
    {"ref_write", 2, ref_write, ERL_NIF_DIRTY_JOB_CPU_BOUND}, // both of these calls *wait* for a lock
    {"ref_read_with_lock_", 2, ref_read_with_lock_, ERL_NIF_DIRTY_JOB_CPU_BOUND}, // which is potentially waiting
    {"ref_write_with_lock", 2, ref_write_with_lock, ERL_NIF_DIRTY_JOB_CPU_BOUND}
  };

ERL_NIF_INIT(nativerefs,
             nif_funcs,
             load,  // load
             NULL,  // upgrade
             NULL,  // unload
             NULL   // reload - deprecated
             )
