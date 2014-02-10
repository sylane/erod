-module(erod_session_sup).

-behaviour(supervisor).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([start_child/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?SESSION_SUP}, ?MODULE, []).


start_child() ->
    supervisor:start_child(?SESSION_SUP, []).


init([]) ->
    lager:info("Starting session supervisor...", []),
    {ok, { {simple_one_for_one, 5, 10},
          [{erod_session, {erod_session, start_link, []},
            temporary, 5000, worker, [erod_session]}]} }.

