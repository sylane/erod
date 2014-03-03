-module(erdom_document_group_child).

-include("erdom_document.hrl").

-export([encode/2]).


encode(jsx, Child) ->
    #?MODULE{name = Name, presence = Pres, connected = Conn} = Child,
    [{<<"name">>, erodlib_jsx:binary_value(name, Name)},
     {<<"presence">>, erodlib_jsx:atom_value(presence, Pres)},
     {<<"connected">>, erodlib_jsx:bool_value(connected, Conn)}].
