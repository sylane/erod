-module(erod_message_reconnect_request).

-include("erod_internal.hrl").

-export([decode/2]).


decode(props, Props) ->
    Session = erod_props:get_binary(session, Props),
    #?MsgRecReq{session = Session}.
