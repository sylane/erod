-module(erdom_group_content).

-include("erdom_internal.hrl").

-export([encode/2]).


encode(jsx, #erdom_group_content{name = N}) ->
    [{<<"name">>, erodlib_jsx:binary_value(name, N)}].
