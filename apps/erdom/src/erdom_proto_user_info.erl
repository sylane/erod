-module(erdom_proto_user_info).

-include("erdom_user.hrl").

-export([encode/2]).


encode(jsx, Result) ->
    #?MODULE{session = Session, self = Self, rooms = Rooms, fav = Fav,
             root = Root, home = Home, config = Config} = Result,
    [{<<"session">>, erodlib_jsx:binary_value(session, Session)},
     {<<"self">>, erodlib_jsx:key_value(self, Self)},
     {<<"rooms">>, erodlib_jsx:key_value(rooms, Rooms)},
     {<<"fav">>, erodlib_jsx:key_value(fav, Fav)},
     {<<"root">>, erodlib_jsx:key_value(root, Root)},
     {<<"home">>, erodlib_jsx:key_value(home, Home)},
     {<<"config">>, erodlib_jsx:struct_value(config, Config)}].

