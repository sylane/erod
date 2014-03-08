-module(erdom_document_group_content).

-include("erdom_document.hrl").

-export([encode/2]).


encode(jsx, Content) ->
    #?MODULE{name = N} = Content,
    [{<<"name">>, erodlib_jsx:encode_str(name, N)}].
