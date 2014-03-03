-module(erdom_document_group_content).

-include("erdom_document.hrl").

-export([encode/2]).


encode(jsx, Content) ->
    #?MODULE{name = N} = Content,
    [{<<"name">>, erodlib_jsx:binary_value(name, N)}].
