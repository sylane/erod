-module(erod_message_login_request).

-include("erodws_internal.hrl").

-export([decode/2]).

-export([as_identity/1]).
-export([as_credential/1]).


decode(props, Props) ->
    Username = erod_props:get_binary(username, Props),
    Password = erod_props:get_binary(password, Props),
    #?MsgLogReq{username = Username, password = Password}.


as_identity(#?MsgLogReq{username = Username}) ->
    #?UserIdent{username = Username}.


as_credential(#?MsgLogReq{username = Username, password = Password}) ->
    #?UserCred{identity = #?UserIdent{username = Username},
               password = Password}.
