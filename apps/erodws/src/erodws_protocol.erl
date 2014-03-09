%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erodws.
%%%
%%% Erodws is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% ==========================================================================
%%% @copyright 2014 Sebastien Merle <s.merle@gmail.com>
%%% @author Sebastien Merle <s.merle@gmail.com>
%%% @doc TODO: Document module erodws_protocol.
%%% @end
%%% ==========================================================================

-module(erodws_protocol).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erodws_protocol.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/3,
         attach_context/2,
         release_context/1,
         shutdown/2,
         terminated/2,
         handle_packet/3,
         encode_error_reply/4,
         encode_error_reply/5,
         encode_result_reply/5,
         encode_result_reply/6,
         encode_error/4]).


%%% ==========================================================================
%%% Maros
%%% ==========================================================================

-define(Proto, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?Proto, {peer :: {inet:ip_address(), inet:port_number()},
                 ctx :: erod:context() | undefined,
                 mod :: module(),
                 state :: term()}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type protocol() :: #?Proto{}.
-export_type([protocol/0]).


%%% ==========================================================================
%%% Behaviour erodws_protocol Specification
%%% ==========================================================================

-callback init(Req, Options)
    -> {ok, Req, State}
    when Req :: cowboy_req:req(), Options :: term(), State :: term().

-callback decode_login_credential(Fmt, Data, State)
    -> {ok, Identity, Credential, State}
    when Fmt :: atom(), Data :: term(), State :: term(),
         Identity :: term(), Credential :: term().

-callback decode_restore_credential(Fmt, Data, State)
    -> {ok, Identity, Token, State}
    when Fmt :: atom(), Data :: term(), State :: term(),
         Identity :: term(), Token :: term().


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new protocol with specified callack module and options.
%% @end
%% -----------------------------------------------------------------
-spec new(Req, Module, Options)
    -> {ok, Req, Proto}
    when Req :: cowboy_req:req(), Module :: module(),
         Options :: term(), Proto :: protocol().
%% -----------------------------------------------------------------

new(Req, Module, Options) ->
    {Peer, Req2} = cowboy_req:peer(Req),
    Proto = #?Proto{peer = Peer},
    info("Websocket initiated.", [], Proto),
    {ok, Req3, State} = Module:init(Req2, Options),
    {ok, Req3, Proto#?Proto{mod = Module, state = State}}.


%% -----------------------------------------------------------------
%% @doc Attaches a context to the protocol.
%% @end
%% -----------------------------------------------------------------
-spec attach_context(Context, Protocol)
    -> Protocol
    when Context :: erod:context(), Protocol :: protocol().
%% -----------------------------------------------------------------

attach_context(Ctx, #?Proto{ctx = undefined} = Proto) ->
    info("Websocket attached.", [], Ctx),
    Proto#?Proto{ctx = erod_context:compact(Ctx)}.


%% -----------------------------------------------------------------
%% @doc Releases the protocol current context.
%% @end
%% -----------------------------------------------------------------
-spec release_context(Protocol)
    -> Protocol
    when Protocol :: protocol().
%% -----------------------------------------------------------------

release_context(#?Proto{ctx = Ctx} = Proto) ->
    info("Websocket released.", [], Ctx),
    Proto#?Proto{ctx = undefined}.


%% -----------------------------------------------------------------
%% @doc Shutdowns the protocol.
%% @end
%% -----------------------------------------------------------------
-spec shutdown(Reason, Protocol)
    -> Protocol
    when Reason :: term(), Protocol :: protocol().
%% -----------------------------------------------------------------

shutdown(Reason, Proto) ->
    info("Websocket is shutting down: ~p.", [Reason], Proto),
    Proto.


%% -----------------------------------------------------------------
%% @doc Informs the protocol of termination.
%% @end
%% -----------------------------------------------------------------
-spec terminated(Reason, Protocol)
    -> Protocol
    when Reason :: term(), Protocol :: protocol().
%% -----------------------------------------------------------------

terminated(Reason, Proto) ->
    info("Websocket has been terminated: ~p", [Reason], Proto),
    Proto.


%% -----------------------------------------------------------------
%% @doc Handles a protocol packet.
%% @end
%% -----------------------------------------------------------------
-spec handle_packet(Fmt, Data, Protocol)
    -> {ok, Protocol}
     | {error, Reason, Protocol}
    when Fmt :: atom(), Data :: term(),
         Protocol :: protocol(), Reason :: term().
%% -----------------------------------------------------------------

handle_packet(Fmt, Data, Proto) ->
    try erodws_proto_message:decode(Fmt, Data) of
        Msg ->
            {ok, handle_message(Msg, Proto)}
    catch
        _:Reason ->
            error("Websocket failed decoding ~p packet: ~p",
                  [Fmt, Reason], Proto),
            {error, Reason, Proto}
    end.


%% -----------------------------------------------------------------
%% @doc Encodes an error reply to a given message in specified format.
%% @end
%% -----------------------------------------------------------------
-spec encode_error_reply(Fmt, Msg, Error, Protocol)
    -> {ok, Data, Protocol}
    when Fmt :: atom(), Msg :: erodws_message(),
         Error :: term(), Data :: term(), Protocol :: protocol().
%% -----------------------------------------------------------------

encode_error_reply(Fmt, #?Msg{type = request, id = Id, cls = Cls}, Error, Proto) ->
    {ok, erodws_proto_message:encode_error_reply(Fmt, Cls, Id, Error), Proto}.


%% -----------------------------------------------------------------
%% @doc Encodes an error reply with given identifier in specified format.
%% @end
%% -----------------------------------------------------------------
-spec encode_error_reply(Fmt, Cls, Id, Error, Protocol)
    -> {ok, Data, Protocol}
    when Fmt :: atom(), Cls :: atom(), Id :: erodws_request_id(),
         Error :: term(), Data :: term(), Protocol :: protocol().
%% -----------------------------------------------------------------

encode_error_reply(Fmt, Cls, Id, Error, Proto) ->
    {ok, erodws_proto_message:encode_error_reply(Fmt, Cls, Id, Error), Proto}.


%% -----------------------------------------------------------------
%% @doc Encodes a result reply to a given message in specified format.
%% @end
%% -----------------------------------------------------------------
-spec encode_result_reply(Fmt, Msg, Status, Result, Protocol)
    -> {ok, Data, Protocol}
    when Fmt :: atom(), Msg :: erodws_message(), Status :: pos_integer(),
         Result :: term(), Data :: term(), Protocol :: protocol().
%% -----------------------------------------------------------------

encode_result_reply(Fmt, Msg, Status, Res, Proto) ->
    #?Msg{type = request, id = Id, cls = Cls} = Msg,
    Pkt = erodws_proto_message:encode_result_reply(Fmt, Cls, Id, Status, Res),
    {ok, Pkt, Proto}.


%% -----------------------------------------------------------------
%% @doc Encodes a result reply with given identifier in specified format.
%% @end
%% -----------------------------------------------------------------
-spec encode_result_reply(Fmt, Cls, Id, Status, Result, Protocol)
    -> {ok, Data, Protocol}
    when Fmt :: atom(), Cls :: atom(), Id :: erodws_request_id(),
         Status :: pos_integer(), Result :: term(),
         Data :: term(), Protocol :: protocol().
%% -----------------------------------------------------------------

encode_result_reply(Fmt, Cls, Id, Status, Res, Proto) ->
    Pkt = erodws_proto_message:encode_result_reply(Fmt, Cls, Id, Status, Res),
    {ok, Pkt, Proto}.


%% -----------------------------------------------------------------
%% @doc Encodes an error in specified format.
%% @end
%% -----------------------------------------------------------------
-spec encode_error(Fmt, Cls, Error, Protocol)
    -> {ok, Data, Protocol}
    when Fmt :: atom(), Cls :: atom(),
         Error :: term(), Data :: term(), Protocol :: protocol().
%% -----------------------------------------------------------------

encode_error(Fmt, Cls, Error, Proto) ->
    {ok, erodws_proto_message:encode_error(Fmt, Cls, Error), Proto}.


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

handle_message(Msg, #?Proto{ctx = undefined, peer = Peer} = Proto) ->
    Ctx = erodws_context:new(self(), Peer, Msg),
    handle_message(Msg, Ctx, Proto);

handle_message(Msg, #?Proto{ctx = RefCtx, peer = Peer} = Proto) ->
    Ctx = erodws_context:clone(self(), Peer, Msg, RefCtx),
    handle_message(Msg, Ctx, Proto).


handle_message(Msg, Ctx, Proto) ->
    #?Msg{type = Type, cls = Cls, id = Id, data = Data} = Msg,
    debug("Websocket received ~p ~p: ~p", [Cls, Type, Data], Ctx),
    try handle_message(Type, Cls, Id, Data, Ctx, Proto)
    catch _:Reason -> handle_error(Msg, Reason, Ctx, Proto)
    end.


handle_error(#?Msg{type = Type, cls = Cls}, Reason, Ctx, Proto) ->
    warning("Websocked failed processing ~p ~p: ~p", [Cls, Type, Reason], Ctx),
    erod_context:failed(Reason, Ctx),
    Proto.


handle_message(request, get_content, _, Data, Ctx, Proto) ->
    #erodws_proto_get_content_request{key = K, ver = V, subscribe = S} = Data,
    erod_actions:perform(get_content, [K, V, S], Ctx),
    Proto;

handle_message(request, get_children, _, Data, Ctx, Proto) ->
    #erodws_proto_get_children_request
     {key = K, ver = V, view = W, page = P, subscribe = S} = Data,
    erod_actions:perform(get_children, [K, V, W, P, S], Ctx),
    Proto;

handle_message(request, patch_content, _, Data, Ctx, Proto) ->
    #erodws_proto_patch_content_request{key = K, ver = V, patch = P} = Data,
    erod_actions:perform(patch_content, [K, V, P], Ctx),
    Proto;

handle_message(request, login, _, Data, Ctx, Proto) ->
    {ok, Ident, Cred, NewProto} = decode_login_credential(term, Data, Proto),
    Proxy = erodws_proxy:new(self()),
    Actions = [{login, [Ident, Cred]}, {bind, [Proxy]}],
    erod_actions:perform(Actions, Ctx),
    NewProto;

handle_message(request, reconnect, _, Data, Ctx, Proto) ->
    {ok, Ident, Token, NewProto} = decode_restore_credential(term, Data, Proto),
    Proxy = erodws_proxy:new(self()),
    Actions = [{restore, [Ident, Token]}, {bind, [Proxy]}],
    erod_actions:perform(Actions, Ctx),
    NewProto;

handle_message(request, logout, _, _, Ctx, Proto) ->
    erod_actions:perform(logout, [], Ctx),
    Proto.


decode_login_credential(Fmt, Data, #?Proto{mod = Mod, state = St} = Proto) ->
    {ok, Ident, Cred, NewSt} = Mod:decode_login_credential(Fmt, Data, St),
    {ok, Ident, Cred, Proto#?Proto{state = NewSt}}.


decode_restore_credential(Fmt, Data, #?Proto{mod = Mod, state = St} = Proto) ->
    {ok, Ident, Token, NewSt} = Mod:decode_restore_credential(Fmt, Data, St),
    {ok, Ident, Token, Proto#?Proto{state = NewSt}}.


%%FIXME: Enable when needed or remove.
%% debug(Format, Params, #?Proto{ctx = undefined, peer = Peer}) ->
%%     lager:debug("~s " ++ Format, [log_id(Peer) |Params]);
%%
%% debug(Format, Params, #?Proto{ctx = Ctx}) ->
%%     erod_context:debug(Format, Params, Ctx);

debug(Format, Params, Ctx) ->
    erod_context:debug(Format, Params, Ctx).


info(Format, Params, #?Proto{ctx = undefined, peer = Peer}) ->
    lager:info("~s " ++ Format, [log_id(Peer) |Params]);

info(Format, Params, #?Proto{ctx = Ctx}) ->
    erod_context:info(Format, Params, Ctx);

info(Format, Params, Ctx) ->
    erod_context:info(Format, Params, Ctx).


%%FIME: Enable when needed or remove.
%% warning(Format, Params, #?Proto{ctx = undefined, peer = Peer}) ->
%%     lager:warning("~s " ++ Format, [log_id(Peer) |Params]);
%%
%% warning(Format, Params, #?Proto{ctx = Ctx}) ->
%%     erod_context:warning(Format, Params, Ctx);

warning(Format, Params, Ctx) ->
    erod_context:warning(Format, Params, Ctx).


error(Format, Params, #?Proto{ctx = undefined, peer = Peer}) ->
    lager:error("~s " ++ Format, [log_id(Peer) |Params]);

error(Format, Params, #?Proto{ctx = Ctx}) ->
    erod_context:error(Format, Params, Ctx).

%%FIXME: Enable if needed or remove.
%% error(Format, Params, Ctx) ->
%%     erod_context:error(Format, Params, Ctx).


log_id(Peer) ->
    PeerBin = erodlib:peer2bin(Peer),
    <<"<?,?,", PeerBin/binary, ">">>.
