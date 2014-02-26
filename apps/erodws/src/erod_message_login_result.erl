-module(erod_message_login_result).

-include("erodws_internal.hrl").

-export([encode/2]).


encode(jsx, #?MsgLogRes{} = Res) ->
    [{<<"session">>, erodlib_jsx:binary_value(session, Res#?MsgLogRes.session)},
     {<<"self">>, erodlib_jsx:key_value(self, Res#?MsgLogRes.self)},
     {<<"rooms">>, erodlib_jsx:key_value(rooms, Res#?MsgLogRes.rooms)},
     {<<"fav">>, erodlib_jsx:key_value(fav, Res#?MsgLogRes.fav)},
     {<<"root">>, erodlib_jsx:key_value(root, Res#?MsgLogRes.root)},
     {<<"home">>, erodlib_jsx:key_value(home, Res#?MsgLogRes.home)},
     {<<"config">>, erodlib_jsx:struct_value(config, Res#?MsgLogRes.config)}].

