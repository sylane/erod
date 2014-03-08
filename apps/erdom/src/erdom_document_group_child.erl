-module(erdom_document_group_child).

-include("erdom_document.hrl").

-export([encode/2]).


encode(jsx, Child) ->
    #?MODULE{name = Name, presence = Pres, connected = Conn} = Child,
    [{<<"name">>, erodlib_jsx:encode_str(name, Name)},
     {<<"presence">>, erodlib_jsx:encode_str(presence, Pres)},
     {<<"connected">>, erodlib_jsx:encode_bool(connected, Conn)}].
