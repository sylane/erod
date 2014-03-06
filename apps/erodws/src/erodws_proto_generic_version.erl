-module(erodws_proto_generic_version).

-include("erodws_protocol.hrl").

-export([decode/2,
         encode/2]).


decode(term, Term) ->
    Version = erodlib_term:get_ver(ver, Term, undefined),
    #?MODULE{ver = Version}.


encode(jsx, #?MODULE{ver = Version}) ->
    [{<<"ver">>, erodlib_jsx:ver_value(ver, Version)}].
