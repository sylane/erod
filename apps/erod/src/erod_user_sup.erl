-module(erod_user_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([start_child/1]).

-export([init/1]).

-define(PROCESS, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?PROCESS}, ?MODULE, []).


start_child(UserIdent) ->
    supervisor:start_child(?PROCESS, [UserIdent]).


init([]) ->
    lager:info("Starting user supervisor...", []),
    {ok, { {simple_one_for_one, 5, 10},
          [{erod_user, {erod_user, start_link, []},
            transient, 5000, worker, [erod_user]}]} }.

