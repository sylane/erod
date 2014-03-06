-module(erodws_proto_patch_content_request).

-include("erodws_protocol.hrl").

-export([decode/2]).


decode(term, Term) ->
    Key = erodlib_term:get_key(key, Term),
    Ver = erodlib_term:get_ver(ver, Term, undefined),
    Patch = erodlib_term:get_patch(patch, Term),
    #?MODULE{key = Key, ver = Ver, format = term, patch = Patch}.
