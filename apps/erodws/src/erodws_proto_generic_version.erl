-module(erodws_proto_generic_version).

-include("erodws_protocol.hrl").

-export([decode/2,
         encode/2]).


decode(term, Term) ->
    erodlib_term:get_ver(ver, Term, undefined).


encode(jsx, Version) ->
    [{<<"ver">>, erodlib_jsx:ver_value(ver, Version)}].
