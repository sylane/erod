-module(erod_message_login_result).

-include("erod_internal.hrl").

-export([encode/2]).


encode(jsx, #?MsgLogRes{} = Res) ->
    [{<<"sesion">>, erod_jsx:binary_value(session, Res#?MsgLogRes.session)},
     {<<"self">>, erod_jsx:struct_value(self, Res#?MsgLogRes.self)},
     {<<"rooms">>, erod_jsx:struct_value(rooms, Res#?MsgLogRes.rooms)},
     {<<"fav">>, erod_jsx:struct_value(fav, Res#?MsgLogRes.fav)},
     {<<"root">>, erod_jsx:struct_value(root, Res#?MsgLogRes.root)},
     {<<"home">>, erod_jsx:struct_value(home, Res#?MsgLogRes.home)},
     {<<"config">>, erod_jsx:struct_value(config, Res#?MsgLogRes.config)}].

