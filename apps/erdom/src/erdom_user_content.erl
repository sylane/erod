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
    [{<<"first_name">>, erod_jsx:binary_value(first_name, FN)},
     {<<"last_name">>, erod_jsx:binary_value(last_name, LN)},
     {<<"display_name">>, erod_jsx:binary_value(display_name, DN)},
     {<<"picture">>, erod_jsx:binary_value(picture, Pic)},
     {<<"presence">>, erod_jsx:atom_value(presence, Pres)},
     {<<"connected">>, erod_jsx:bool_value(connected, Conn)}].
