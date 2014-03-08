-module(erdom_document_user_content).

-include("erdom_document.hrl").

-export([encode/2]).


encode(jsx, Content) ->
    #?MODULE{first_name = FN, last_name = LN, display_name = DN,
             picture = Pic, presence = Pres, connected = Conn} = Content,
    [{<<"first_name">>, erodlib_jsx:encode_str(first_name, FN)},
     {<<"last_name">>, erodlib_jsx:encode_str(last_name, LN)},
     {<<"display_name">>, erodlib_jsx:encode_str(display_name, DN)},
     {<<"picture">>, erodlib_jsx:encode_str(picture, Pic)},
     {<<"presence">>, erodlib_jsx:encode_str(presence, Pres)},
     {<<"connected">>, erodlib_jsx:encode_bool(connected, Conn)}].
