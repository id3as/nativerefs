#include <erl_nif.h>

typedef struct _nif_globals {
  ErlNifEnv *global_env;
  ErlNifResourceType *ref_resource_type;
  ErlNifResourceType *ref_lock_resource_type;
} nif_globals;

typedef struct _ref {
  ERL_NIF_TERM term;
  ErlNifMutex* mutex;
} ref;

typedef struct _ref_lock {
  ref* ref;
} ref_lock;

static ERL_NIF_TERM ref_new(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = (ref *) enif_alloc_resource(globals->ref_resource_type, sizeof(ref));
  r->term = enif_make_copy(globals->global_env, argv[0]);
  r->mutex = enif_mutex_create("native_ref_lock");
  ERL_NIF_TERM resource = enif_make_resource(env, r);
  enif_release_resource(r);
  return enif_make_tuple2(env, enif_make_atom(env, "ok"), resource);
}

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

static ERL_NIF_TERM ref_read_with_lock(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  if(argc != 1) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "bad_arity")); 
  }

  nif_globals *globals = (nif_globals *) enif_priv_data(env);
  ref *r = NULL;
  
  if(!enif_get_resource(env, argv[0], globals->ref_resource_type, (void**)&r)) {
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "badarg")); 
  }

  ref_lock *l = (ref_lock *) enif_alloc_resource(globals->ref_lock_resource_type, sizeof(ref_lock));
  l->ref = r;
  enif_mutex_lock(r->mutex);
  enif_keep_resource(r);
  ERL_NIF_TERM resource = enif_make_resource(env, l);
  enif_release_resource(l);
  return enif_make_tuple3(env, enif_make_atom(env, "ok"), resource, enif_make_copy(env, r->term));
}

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

  l->ref->term = enif_make_copy(globals->global_env, argv[1]);
  enif_release_resource(l->ref);

  enif_mutex_unlock(l->ref->mutex);
  l->ref = NULL;
  
  return enif_make_atom(env, "ok");
}

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
  r->term = enif_make_copy(globals->global_env, argv[1]);
  enif_mutex_unlock(r->mutex);
  
  return enif_make_atom(env, "ok");
}

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
    enif_release_resource(l->ref);
    enif_mutex_unlock(l->ref->mutex);
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

  return 0;
}

static ErlNifFunc nif_funcs[] =
  {
    {"ref_new", 1, ref_new},
    {"ref_read", 1, ref_read},
    {"ref_try_write", 2, ref_try_write},
    {"ref_write", 2, ref_write, ERL_NIF_DIRTY_JOB_CPU_BOUND},
    {"ref_read_with_lock", 1, ref_read_with_lock},
    {"ref_write_with_lock", 2, ref_write_with_lock}
  };

ERL_NIF_INIT(nativerefs,
             nif_funcs,
             load,  // load
             NULL,  // upgrade
             NULL,  // unload
             NULL   // reload - deprecated
             )
