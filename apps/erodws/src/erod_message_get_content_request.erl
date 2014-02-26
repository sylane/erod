-module(erod_message_get_content_request).

-include("erodws_internal.hrl").

-export([decode/2]).


decode(props, Props) ->
    Key = erodlib_props:get_key(key, Props),
    Ver = erodlib_props:get_ver(ver, Props, undefined),
    Subscribe = erodlib_props:get_bool(subscribe, Props, false),
    #?MsgGetConReq{key = Key, ver = Ver, subscribe = Subscribe}.
