-module(erod_message_get_children_request).

-include("erodws_internal.hrl").

-export([decode/2]).


decode(props, Props) ->
    Key = erodlib_props:get_key(key, Props),
    Ver = erodlib_props:get_ver(ver, Props, undefined),
    ViewId = erodlib_props:get_atom(view, Props),
    PageId = erodlib_props:get_integer(page, Props),
    Subscribe = erodlib_props:get_bool(subscribe, Props, false),
    #?MsgGetChiReq{key = Key, ver = Ver, view = ViewId,
                   page = PageId, subscribe = Subscribe}.
