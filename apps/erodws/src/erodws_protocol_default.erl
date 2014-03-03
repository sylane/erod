-module(erodws_protocol_default).

-behaviour(erodws_protocol).

-export([init/2,
         decode_login_credential/3,
         decode_restore_credential/3]).

-define(St, ?MODULE).
-record(?St, {}).


init(Req, []) ->
    {ok, Req, #?St{}}.


decode_login_credential(Fmt, Data, State) ->
    {Ident, Cred} = erodws_proto_login_credential:decode(Fmt, Data),
    {ok, Ident, Cred, State}.


decode_restore_credential(Fmt, Data, State) ->
    {Ident, Token} = erodws_proto_restore_credential:decode(Fmt, Data),
    {ok, Ident, Token, State}.



