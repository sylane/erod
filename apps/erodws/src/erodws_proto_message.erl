-module(erodws_proto_message).

-include("erodws_protocol.hrl").

-export([decode/2,
         encode/2,
         encode_error/3,
         encode_error_reply/4,
         encode_result_reply/5]).

-define(MSG_TYPES, [request, result, error, notify]).


decode(json, Json) ->
    decode(jsx, erodlib_jsx:decode(json, Json));

decode(jsx, Jsx) ->
    case erodlib_term:decode(jsx, Jsx) of
        [T |_] = Term when is_tuple(T) ->
            decode(term, Term);
        _Any -> error({format_error, bad_structure})
    end;

decode(term, Term) ->
    Type = erodlib_term:get_allowed_atom(type, Term, ?MSG_TYPES),
    Id = decode_id(Type, Term),
    Cls = erodlib_term:get_atom(cls, Term),
    Data = decode_data(term, Type, Cls, Term),
    #?Msg{type = Type, id = Id, cls = Cls, data = Data};

decode(Fmt, _Any) ->
    error({format_error, {unsupported_format, Fmt}}).


encode(json, Msg) ->
    erodlib_jsx:encode(json, encode(jsx, Msg));

encode(jsx, #?Msg{type = T, id = I, cls = C, status = S, data = D}) ->
    encode_message(jsx, T, C, I, S, encode_data(jsx, T, C, D));

encode(Fmt, _Any) ->
    error({format_error, {unsupported_format, Fmt}}).


encode_error(json, Cls, Error) ->
    erodlib_jsx:encode(json, encode_error(jsx, Cls, Error));

encode_error(Fmt, Cls, Error) ->
    ErrorRec = erodws_errors:map(Cls, Error),
    Status = ErrorRec#erodws_proto_generic_error.status,
    Data = erodws_proto_generic_error:encode(Fmt, ErrorRec),
    encode_message(Fmt, error, Cls, undefined, Status, Data).


encode_error_reply(json, Cls, Id, Error) ->
    erodlib_jsx:encode(json, encode_error_reply(jsx, Cls, Id, Error));

encode_error_reply(Fmt, Cls, Id, Error) ->
    ErrorRec = erodws_errors:map(Cls, Error),
    Status = ErrorRec#erodws_proto_generic_error.status,
    Data = erodws_proto_generic_error:encode(Fmt, ErrorRec),
    encode_message(Fmt, error, Cls, Id, Status, Data).


encode_result_reply(json, Cls, Id, Status, Result) ->
    erodlib_jsx:encode(json, encode_result_reply(jsx, Cls, Id, Status, Result));

encode_result_reply(Fmt, Cls, Id, Status, Result) ->
    Data = encode_data(Fmt, result, Cls, Result),
    encode_message(Fmt, result, Cls, Id, Status, Data).



decode_id(request, Term) ->
    erodlib_term:get_binary(id, Term);

decode_id(result, Term) ->
    erodlib_term:get_binary(id, Term);

decode_id(error, Term) ->
    erodlib_term:get_binary(id, Term, undefined);

decode_id(notify, Term) ->
    erodlib_term:ensure_undefined(id, Term).


decode_data(term, request, get_content, Term) ->
    RawData = erodlib_term:get_struct(data, Term),
    erodws_proto_get_content_request:decode(term, RawData);

decode_data(term, request, get_children, Term) ->
    RawData = erodlib_term:get_struct(data, Term),
    erodws_proto_get_children_request:decode(term, RawData);

decode_data(term, request, patch_content, Term) ->
    RawData = erodlib_term:get_struct(data, Term),
    erodws_proto_patch_content_request:decode(term, RawData);

decode_data(term, request, login, Term) ->
    erodlib_term:get_struct(data, Term, undefined);

decode_data(term, request, logout, Term) ->
    erodlib_term:ensure_undefined(data, Term);

decode_data(term, request, reconnect, Term) ->
    erodlib_term:get_struct(data, Term, undefined);

decode_data(term, _Type, _Cls, _Term) ->
    error(message_class_unknown).


encode_data(Fmt, result, get_content, Data) ->
    erodws_proto_get_content_result:encode(Fmt, Data);

encode_data(Fmt, result, get_children, Data) ->
    erodws_proto_get_children_result:encode(Fmt, Data);

encode_data(Fmt, result, patch_content, Data) ->
    erodws_proto_generic_version:encode(Fmt, Data);

encode_data(jsx, result, login, Data) ->
    erodlib_jsx:struct_value(data, Data);

encode_data(_Fmt, result, C, undefined)
  when C =:= reconnect; C =:= logout ->
    undefined;

encode_data(_Fmt, result, C, _Any)
  when C =:= reconnect; C =:= logout ->
    error({format_error, {value_not_allowed, data}});

encode_data(Fmt, error, _Cls, #erodws_proto_generic_error{} = Data) ->
    erodws_proto_generic_error:encode(Fmt, Data);

encode_data(Fmt, error, Cls, Data) ->
    ErrorRec = erodws_errors:map(Cls, Data),
    erodws_proto_generic_error:encode(Fmt, ErrorRec);

encode_data(_Fmt, _Type, _Cls, _Data) ->
    error(message_class_unknown).


%%TODO: Enable when needed or delete.
%% encode_message(jsx, request, C, I, undefined, undefined) ->
%%     [{<<"type">>, <<"request">>},
%%      {<<"id">>, erodlib_jsx:binary_value(id, I)},
%%      {<<"cls">>, erodlib_jsx:atom_value(cls, C)}];
%%
%% encode_message(jsx, request, C, I, undefined, D) ->
%%     [{<<"type">>, <<"request">>},
%%      {<<"id">>, erodlib_jsx:binary_value(id, I)},
%%      {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
%%      {<<"data">>, D}];

encode_message(jsx, result, C, I, S, undefined) ->
    [{<<"type">>, <<"result">>},
     {<<"id">>, erodlib_jsx:binary_value(id, I)},
     {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
     {<<"status">>, erodlib_jsx:integer_value(cls, S)}];

encode_message(jsx, result, C, I, S, D) ->
    [{<<"type">>, <<"result">>},
     {<<"id">>, erodlib_jsx:binary_value(id, I)},
     {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
     {<<"status">>, erodlib_jsx:integer_value(cls, S)},
     {<<"data">>, D}];

encode_message(jsx, error, C, undefined, S, undefined) ->
    [{<<"type">>, <<"error">>},
     {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
     {<<"status">>, erodlib_jsx:integer_value(cls, S)}];

encode_message(jsx, error, C, undefined, S, D) ->
    [{<<"type">>, <<"error">>},
     {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
     {<<"status">>, erodlib_jsx:integer_value(cls, S)},
     {<<"data">>, D}];

encode_message(jsx, error, C, I, S, undefined) ->
    [{<<"type">>, <<"error">>},
     {<<"id">>, erodlib_jsx:binary_value(id, I)},
     {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
     {<<"status">>, erodlib_jsx:integer_value(cls, S)}];

encode_message(jsx, error, C, I, S, D) ->
    [{<<"type">>, <<"error">>},
     {<<"id">>, erodlib_jsx:binary_value(id, I)},
     {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
     {<<"status">>, erodlib_jsx:integer_value(cls, S)},
     {<<"data">>, D}];

%%%TODO: Enable when supporting notifications
%% encode_message(jsx, notify, C, undefined, undefined, undefined) ->
%%     [{<<"type">>, <<"notify">>},
%%      {<<"cls">>, erodlib_jsx:atom_value(cls, C)}];
%%
%% encode_message(jsx, notify, C, undefined, undefined, D) ->
%%     [{<<"type">>, <<"notify">>},
%%      {<<"cls">>, erodlib_jsx:atom_value(cls, C)},
%%      {<<"data">>, D}];
%%
%% encode_message(jsx, notify, _, _, _) ->
%%     error({format_error, {value_not_allowed, id}});

encode_message(jsx, _, _, _, _, _) ->
    error({format_error, {value_not_allowed, type}}).



