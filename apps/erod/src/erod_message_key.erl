-module(erod_message_key).

-include("erod_internal.hrl").

-export([decode/2]).
-export([encode/2]).


decode(props, Props) ->
    Type = erod_props:get_atom(type, Props),
    Id = erod_props:get_struct(id, Props, undefined),
    #?MsgKey{type = Type, id = Id}.


encode(jsx, #?MsgKey{type = Type, id = undefined}) ->
    [{<<"type">>, erod_jsx:atom_value(type, Type)}];

encode(jsx, #?MsgKey{type = Type, id = Id}) ->
    [{<<"type">>, erod_jsx:atom_value(type, Type)},
     {<<"id">>, erod_jsx:struct_value(id, Id)}].
