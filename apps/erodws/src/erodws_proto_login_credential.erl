-module(erodws_proto_login_credential).

-include("erodws_protocol.hrl").

-export([decode/2]).


decode(term, Term) ->
    Username = erodlib_term:get_binary(username, Term),
    Password = erodlib_term:get_binary(password, Term),
    {Username, {Username, Password}}.
