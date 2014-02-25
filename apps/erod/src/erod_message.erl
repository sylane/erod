-module(erod_message).

-include("erod_internal.hrl").

-export([decode/2]).
-export([encode/2]).
-export([encode_error/3, encode_error/4]).
-export([encode_error_reply/3]).

-define(MSG_TYPES, [request, result, error, notify]).


decode(json, Json) ->
    decode(jsx, decode_json(Json));

decode(jsx, Jsx) ->
    case erod_props:decode(jsx, Jsx) of
        [T |_] = Props when is_tuple(T) ->
            decode(props, Props);
        _Any -> error({format_error, bad_structure})
    end;

decode(props, Props) ->
    Type = erod_props:get_allowed_atom(type, Props, ?MSG_TYPES),
    Id = get_id(Props, Type),
    Cls = erod_props:get_atom(cls, Props),
    Data = erod_props:get_struct(data, Props, undefined),
    #?Msg{type = Type, id = Id, cls = Cls, data = Data}.


encode(json, Msg) ->
    try jsx:encode(encode(jsx, Msg))
    catch error:badarg -> error({format_error, bad_structure})
    end;

encode(jsx, #?Msg{type = Type, data = undefined} = Req)
  when Type =:= request; Type =:= result ->
    [{<<"type">>, erod_jsx:atom_value(type, Type)},
     {<<"id">>, erod_jsx:binary_value(id, Req#?Msg.id)},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)}];

encode(jsx, #?Msg{type = Type} = Req)
  when Type =:= request; Type =:= result ->
    [{<<"type">>, erod_jsx:atom_value(type, Type)},
     {<<"id">>, erod_jsx:binary_value(id, Req#?Msg.id)},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)},
     {<<"data">>, erod_jsx:struct_value(data, Req#?Msg.data)}];

encode(jsx, #?Msg{type = error, id = undefined, data = undefined} = Req) ->
    [{<<"type">>, <<"error">>},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)}];

encode(jsx, #?Msg{type = error, id = undefined} = Req) ->
    [{<<"type">>, <<"error">>},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)},
     {<<"data">>, erod_jsx:struct_value(data, Req#?Msg.data)}];

encode(jsx, #?Msg{type = error, data = undefined} = Req) ->
    [{<<"type">>, <<"error">>},
     {<<"id">>, erod_jsx:binary_value(id, Req#?Msg.id)},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)}];

encode(jsx, #?Msg{type = error} = Req) ->
    [{<<"type">>, <<"error">>},
     {<<"id">>, erod_jsx:binary_value(id, Req#?Msg.id)},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)},
     {<<"data">>, erod_jsx:struct_value(data, Req#?Msg.data)}];

encode(jsx, #?Msg{type = notify, id = undefined, data = undefined} = Req) ->
    [{<<"type">>, <<"notify">>},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)}];

encode(jsx, #?Msg{type = notify, id = undefined} = Req) ->
    [{<<"type">>, <<"notify">>},
     {<<"cls">>, erod_jsx:atom_value(cls, Req#?Msg.cls)},
     {<<"data">>, erod_jsx:struct_value(data, Req#?Msg.data)}];

encode(jsx, #?Msg{type = notify}) ->
    error({format_error, {value_not_allowed, id}});

encode(jsx, _Req) ->
    error({format_error, {value_not_allowed, type}}).


encode_error(Format, Class, Error) ->
    encode_error(Format, Class, undefined, Error).


encode_error(json, Class, Id, Error) ->
    jsx:encode(encode_error(jsx, Class, Id, Error));

encode_error(Format, Class, Id, Error) ->
    encode_error_message(Format, Class, Id, erod_errors:map(Error)).


encode_error_reply(Format, #?Msg{type = request, id = Id, cls = Cls}, Error) ->
    erod_message:encode_error(Format, Cls, Id, Error).


decode_json(Json) ->
    try jsx:decode(Json) of
        {incomplete, _} -> error({format_error, incomplete_json});
        Jsx -> Jsx
    catch error:badarg ->
              error({format_error, bad_json})
    end.


get_id(Props, request) ->
    erod_props:get_binary(id, Props);

get_id(Props, result) ->
    erod_props:get_binary(id, Props);

get_id(Props, error) ->
    erod_props:get_binary(id, Props, undefined);

get_id(Props, notify) ->
    erod_props:ensure_undefined(id, Props).


encode_error_message(jsx, Class, undefined, ErrorData) ->
    [{<<"type">>, <<"error">>},
     {<<"cls">>, erod_jsx:atom_value(cls, Class)},
     {<<"data">>, encode_error_data(jsx, ErrorData)}];

encode_error_message(jsx, Class, Id, ErrorData) ->
    [{<<"type">>, <<"error">>},
     {<<"id">>, erod_jsx:binary_value(id, Id)},
     {<<"cls">>, erod_jsx:atom_value(cls, Class)},
     {<<"data">>, encode_error_data(jsx, ErrorData)}].


%% encode_error_data(jsx, {Code, undefined, undefined}) ->
%%     [{<<"code">>, erod_jsx:integer_value(code, Code)}];
%%
%% encode_error_data(jsx, {Code, Msg, undefined}) ->
%%     [{<<"code">>, erod_jsx:integer_value(code, Code)},
%%      {<<"msg">>, erod_jsx:binary_value(code, Msg)}];
%%
%% encode_error_data(jsx, {Code, undefined, Debug}) ->
%%     DebugData = erlang:iolist_to_binary(io_lib:format("~w", [Debug])),
%%     [{<<"code">>, erod_jsx:integer_value(code, Code)},
%%      {<<"debug">>, erod_jsx:binary_value(debug, DebugData)}];

encode_error_data(jsx, {Code, Msg, Debug}) ->
    DebugData = erlang:iolist_to_binary(io_lib:format("~p", [Debug])),
    [{<<"code">>, erod_jsx:integer_value(code, Code)},
     {<<"msg">>, erod_jsx:binary_value(code, Msg)},
     {<<"debug">>, erod_jsx:binary_value(debug, DebugData)}].
