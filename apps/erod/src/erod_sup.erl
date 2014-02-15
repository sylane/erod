-module(erod_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


start_link() ->
    supervisor:start_link(?MODULE, []).


init([]) ->
    lager:debug("Starting root supervisor...", []),
    {ok, { {one_for_one, 5, 10},
           [?CHILD(erod_users_sup, supervisor),
            ?CHILD(erod_sessions_sup, supervisor),
            ?CHILD(erod_documents_sup, supervisor)]}}.

