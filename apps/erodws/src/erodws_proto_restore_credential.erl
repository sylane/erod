-module(erodws_proto_restore_credential).

-include("erodws_protocol.hrl").

-export([decode/2]).


decode(term, Term) ->
    {undefined, erodlib_term:get_binary(session, Term)}.
