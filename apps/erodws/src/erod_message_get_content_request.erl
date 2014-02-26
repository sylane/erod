-module(erod_message_get_content_request).

-include("erodws_internal.hrl").

-export([decode/2]).


decode(props, Props) ->
    Key = erod_props:get_key(key, Props),
    Ver = erod_props:get_ver(ver, Props, undefined),
    Subscribe = erod_props:get_bool(subscribe, Props, false),
    #?MsgGetConReq{key = Key, ver = Ver, subscribe = Subscribe}.
