-module(erodws_proto_get_children_request).

-include("erodws_protocol.hrl").

-export([decode/2]).


decode(term, Term) ->
    Key = erodlib_term:get_key(key, Term),
    Ver = erodlib_term:get_ver(ver, Term, undefined),
    ViewId = erodlib_term:get_atom(view, Term),
    PageId = erodlib_term:get_integer(page, Term),
    Subscribe = erodlib_term:get_bool(subscribe, Term, false),
    #?MODULE{key = Key, ver = Ver, view = ViewId,
             page = PageId, subscribe = Subscribe}.
