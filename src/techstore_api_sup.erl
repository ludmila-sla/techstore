-module(techstore_api_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    Procs = [
        {mdbcc, {mdbcc, start_link, [techstore:mdbcc()]}, permanent, 60000, worker, []},
        {service, {r_service, start_link, []}, permanent, 60000, worker, []}
    ],
    {ok, {{one_for_one, 1, 5}, Procs}}.
