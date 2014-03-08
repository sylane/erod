-module(erdom_proto_user_info).

-include("erdom_user.hrl").

-export([encode/2]).


encode(jsx, Result) ->
    #?MODULE{session = Session, self = Self, rooms = Rooms, fav = Fav,
             root = Root, home = Home, config = Config} = Result,
    [{<<"session">>, erodlib_jsx:encode_str(session, Session)},
     {<<"self">>, erodlib_jsx:encode_key(self, Self)},
     {<<"rooms">>, erodlib_jsx:encode_key(rooms, Rooms)},
     {<<"fav">>, erodlib_jsx:encode_key(fav, Fav)},
     {<<"root">>, erodlib_jsx:encode_key(root, Root)},
     {<<"home">>, erodlib_jsx:encode_key(home, Home)},
     {<<"config">>, erodlib_jsx:encode_any(config, Config)}].

