-module(nativerefs).

-export([ ref_new/1
        , ref_read/1
        , ref_write/2
        , ref_try_write/2
        , ref_try_read_with_lock/1
        , ref_read_with_lock/1
        , ref_write_with_lock/2
        ]).

-type native_ref() :: reference().
-type native_ref_lock() :: reference().

-type native_ref_error() :: { atom(), atom() }.

-on_load(init/0).

init() ->
  PrivDir = code:priv_dir(nativerefs),
  File = filename:join([PrivDir, ?MODULE]),

  ok = erlang:load_nif(File, 0).

-spec ref_new(term()) -> { ok, native_ref() } | native_ref_error().
ref_new(_Value) ->
  erlang:nif_error(nif_library_not_loaded).

-spec ref_read(native_ref()) -> { ok, term() } | native_ref_error().
ref_read(_Ref) ->
  erlang:nif_error(nif_library_not_loaded).

-spec ref_write(native_ref(), term()) -> ok | native_ref_error().
ref_write(_Ref, _Value) ->
  erlang:nif_error(nif_library_not_loaded).

-spec ref_try_write(native_ref(), term()) -> ok | busy | native_ref_error().
ref_try_write(_Ref, _Value) ->
  erlang:nif_error(nif_library_not_loaded).

-spec ref_try_read_with_lock(native_ref()) -> { ok, native_ref_lock(), term() } | busy | native_ref_error().
ref_try_read_with_lock(_Ref) ->
  erlang:nif_error(nif_library_not_loaded).

-spec ref_read_with_lock(native_ref()) -> { ok, native_ref_lock(), term() } | native_ref_error().
ref_read_with_lock(Ref) ->
  ref_read_with_lock(Ref, 0).

ref_read_with_lock(Ref, N) when N < 5 ->
  case ref_read_with_lock_(Ref, undefined) of
    busy ->
      timer:sleep(0),
      ref_read_with_lock(Ref, N + 1);
    Other ->
      Other
  end;

ref_read_with_lock(Ref, N) ->

  MsgRef = make_ref(),
  case ref_read_with_lock_(Ref, MsgRef) of
    busy ->
      receive
        MsgRef ->
          ref_read_with_lock(Ref, N)
      end;
    Other ->
      Other
  end.

ref_read_with_lock_(_Ref, _MsgRef) ->
  erlang:nif_error(nif_library_not_loaded).

-spec ref_write_with_lock(native_ref_lock(), term()) -> ok | native_ref_error().
ref_write_with_lock(_Lock, _Value) ->
  erlang:nif_error(nif_library_not_loaded).

