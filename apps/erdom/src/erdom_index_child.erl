-module(erdom_index_child).

-include("erdom_internal.hrl").

-export([encode/2]).

encode(jsx, #erdom_index_child{name = Name}) ->
    [{<<"name">>, erod_jsx:binary_value(name, Name)}].
