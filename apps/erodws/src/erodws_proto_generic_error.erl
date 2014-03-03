-module(erodws_proto_generic_error).

-include("erodws_protocol.hrl").

-export([decode/2,
         encode/2]).


decode(term, Term) ->
    Code = erodlib_term:get_integer(code, Term),
    Msg = erodlib_term:get_binary(msg, Term, undefined),
    Debug = erodlib_term:get_binary(debug, Term, undefined),
    #?MODULE{code = Code, msg = Msg, debug = Debug}.


encode(jsx, #?MODULE{code = Code, msg = undefined, debug = undefined}) ->
    [{<<"code">>, erodlib_jsx:integer_value(code, Code)}];

encode(jsx, #?MODULE{code = Code, msg = Msg, debug = undefined}) ->
    [{<<"code">>, erodlib_jsx:integer_value(code, Code)},
     {<<"msg">>, erodlib_jsx:binary_value(code, Msg)}];

encode(jsx, #?MODULE{code = Code, msg = undefined, debug = Debug}) ->
    DebugData = erlang:iolist_to_binary(io_lib:format("~p", [Debug])),
    [{<<"code">>, erodlib_jsx:integer_value(code, Code)},
     {<<"debug">>, erodlib_jsx:binary_value(debug, DebugData)}];

encode(jsx, #?MODULE{code = Code, msg = Msg, debug = Debug}) ->
    DebugData = erlang:iolist_to_binary(io_lib:format("~p", [Debug])),
    [{<<"code">>, erodlib_jsx:integer_value(code, Code)},
     {<<"msg">>, erodlib_jsx:binary_value(code, Msg)},
     {<<"debug">>, erodlib_jsx:binary_value(debug, DebugData)}];

encode(Fmt, Error) ->
    encode(Fmt, erodws_errors:map(Error)).

