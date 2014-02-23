-module(erod_session_sup).

-behaviour(supervisor).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([start_child/1]).

-export([init/1]).

-define(PROCESS, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?PROCESS}, ?MODULE, []).


start_child(SessionToken) ->
    supervisor:start_child(?PROCESS, [SessionToken]).


init([]) ->
    lager:info("Starting session supervisor...", []),
    {ok, { {simple_one_for_one, 5, 10},
          [{erod_session, {erod_session, start_link, []},
            temporary, 5000, worker, [erod_session]}]} }.

