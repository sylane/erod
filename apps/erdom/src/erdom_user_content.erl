-module(erdom_user_content).

-include("erdom_internal.hrl").

-export([encode/2]).


encode(jsx, #erdom_user_content{} = User) ->
    #erdom_user_content{first_name = FN,
                        last_name = LN,
                        display_name = DN,
                        picture = Pic,
                        presence = Pres,
                        connected = Conn} = User,
    [{<<"first_name">>, erodlib_jsx:binary_value(first_name, FN)},
     {<<"last_name">>, erodlib_jsx:binary_value(last_name, LN)},
     {<<"display_name">>, erodlib_jsx:binary_value(display_name, DN)},
     {<<"picture">>, erodlib_jsx:binary_value(picture, Pic)},
     {<<"presence">>, erodlib_jsx:atom_value(presence, Pres)},
     {<<"connected">>, erodlib_jsx:bool_value(connected, Conn)}].
