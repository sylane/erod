-module(erodws_proto_get_content_request).

-include("erodws_protocol.hrl").

-export([decode/2]).


decode(term, Term) ->
    Key = erodlib_term:get_key(key, Term),
    Ver = erodlib_term:get_ver(ver, Term, undefined),
    Subscribe = erodlib_term:get_bool(subscribe, Term, false),
    #?MODULE{key = Key, ver = Ver, subscribe = Subscribe}.
