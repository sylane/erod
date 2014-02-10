-module(erod_sessions_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link(?MODULE, []).


init([]) ->
    lager:info("Starting sessions supervisor...", []),
    {ok, { {rest_for_one, 5, 10},
           [?CHILD(erod_session_manager, worker),
            ?CHILD(erod_session_sup, supervisor)]} }.

