-module(erdom_document_index_child).

-include("erdom_document.hrl").

-export([encode/2]).


encode(jsx, Child) ->
    #?MODULE{name = Name} = Child,
    [{<<"name">>, erodlib_jsx:binary_value(name, Name)}].
