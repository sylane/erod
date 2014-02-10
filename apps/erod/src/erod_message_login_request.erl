-module(erod_message_login_request).

-include("erod_internal.hrl").

-export([decode/2]).
-export([encode/2]).


decode(props, Props) ->
    Username = erod_props:get_binary(username, Props),
    Password = erod_props:get_binary(password, Props),
    #?MsgLogReq{username = Username, password = Password}.


encode(jsx, #?MsgLogReq{username = Username, password = Password}) ->
    [{<<"username">>, erod_jsx:binary_value(username, Username)},
     {<<"password">>, erod_jsx:binary_value(password, Password)}].

