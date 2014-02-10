-module(erod_user_sup).

-behaviour(supervisor).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([init/1]).


start_link() ->
    supervisor:start_link({local, ?USER_SUP}, ?MODULE, []).

init([]) ->
    lager:info("Starting user supervisor...", []),
    {ok, { {simple_one_for_one, 5, 10},
          [{erod_user, {erod_user, start_link, []}, transient, 5000, worker, [erod_user]}]} }.

