-module(erod_message_get_children_request).

-include("erodws_internal.hrl").

-export([decode/2]).


decode(props, Props) ->
    Key = erod_props:get_key(key, Props),
    Ver = erod_props:get_ver(ver, Props, undefined),
    ViewId = erod_props:get_atom(view, Props),
    PageId = erod_props:get_integer(page, Props),
    Subscribe = erod_props:get_bool(subscribe, Props, false),
    #?MsgGetChiReq{key = Key, ver = Ver, view = ViewId,
                   page = PageId, subscribe = Subscribe}.
