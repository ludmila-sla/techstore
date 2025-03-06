-module(tech_settings).

-export([get_env/2]).
-export([mdbcc/0]).
-export([session_ttl/0]).

-export([set_env/2]).

-define(A, ksession).


get_env(K, D) ->
    application:get_env(?A, K, D).

mdbcc() ->
    get_env(mdbcc, <<"mongodb://localhost:27018/techstore">>).
session_ttl() ->
  get_env(session_ttl, 172800).


set_env(K, V) ->
    application:set_env(?A, K, V).