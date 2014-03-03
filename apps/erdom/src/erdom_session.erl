-module(erdom_session).

-behaviour(erod_session).

-export([init/1]).

-define(St, ?MODULE).
-record(?St, {}).


init([]) ->
    #?St{}.
