-module(erod_message_reconnect_request).

-include("erodws_internal.hrl").

-export([decode/2]).


decode(props, Props) ->
    Session = erodlib_props:get_binary(session, Props),
    #?MsgRecReq{session = Session}.
