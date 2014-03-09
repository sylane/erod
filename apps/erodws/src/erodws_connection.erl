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
%%% @doc TODO: Document module erodws_connection.
%%% @end
%%% @private
%%% ==========================================================================

-module(erodws_connection).

-author('Sebastien Merle').

-behaviour(cowboy_websocket_handler).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erodws_protocol.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([notify/4,
         send_error/2,
         send_error_reply/4,
         send_result_reply/5,
         attach_context/2,
         release_context/1,
         close_connection/2]).

%%% Behaviour cowboy_websocket_handler callbacks
-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).
-define(PING_PERIOD, 3000).
-define(INACTIVITY_TIMEOUT, 6000).
-define(CALL_TIMEOUT, 2000).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {proto :: erodws_protocol:protocol(),
              ping_timer :: reference() | undefined}).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Sends a notification through specified connection.
%% @end
%% -----------------------------------------------------------------
-spec notify(Connection, Cls, Fmt, Data) -> ok
    when Connection :: pid(), Cls :: atom(), Fmt :: atom(), Data :: term().
%% -----------------------------------------------------------------

notify(Connection, Cls, Fmt, Data) ->
    Connection ! {notify, Cls, Fmt, Data},
    ok.


%% -----------------------------------------------------------------
%% @doc Sends an error through specified connection.
%% @end
%% -----------------------------------------------------------------
-spec send_error(Connection, Error) -> ok
    when Connection :: pid(), Error :: term().
%% -----------------------------------------------------------------

send_error(Connection, Error) ->
    Connection ! {send_error, Error},
    ok.


%% -----------------------------------------------------------------
%% @doc Sends an error reply through specified connection.
%% @end
%% -----------------------------------------------------------------
-spec send_error_reply(Connection, Cls, Id, Error) -> ok
    when Connection :: pid(), Cls :: atom(), Id :: binary(), Error :: term().
%% -----------------------------------------------------------------

send_error_reply(Connection, Cls, Id, Error) ->
    Connection ! {send_error_reply, Cls, Id, Error},
    ok.


%% -----------------------------------------------------------------
%% @doc Sends a result reply through specified connection.
%% @end
%% -----------------------------------------------------------------
-spec send_result_reply(Connection, Cls, Id, Status, Data) -> ok
    when Connection :: pid(), Cls :: atom(), Id :: binary(),
         Status :: pos_integer(), Data :: term().
%% -----------------------------------------------------------------

send_result_reply(Connection, Cls, Id, Status, Data) ->
    Connection ! {send_result_reply, Cls, Id, Status, Data},
    ok.


%% -----------------------------------------------------------------
%% @doc Attaches a context to the specified connection.
%% @end
%% -----------------------------------------------------------------
-spec attach_context(Connection, Ctx)  -> ok
    when Connection :: pid(), Ctx :: erod:context().
%% -----------------------------------------------------------------

attach_context(Connection, Ctx) ->
    Connection ! {attach_context, Ctx},
    ok.


%% -----------------------------------------------------------------
%% @doc Releases the current context of the specified connection.
%% @end
%% -----------------------------------------------------------------
-spec release_context(Connection)  -> ok
    when Connection :: pid().
%% -----------------------------------------------------------------

release_context(Connection) ->
    Connection ! release_context,
    ok.


%% -----------------------------------------------------------------
%% @doc Closes the specified connection.
%% @end
%% -----------------------------------------------------------------
-spec close_connection(Connection, Reason)  -> ok
    when Connection :: pid(), Reason :: term().
%% -----------------------------------------------------------------

close_connection(Connection, Reason) ->
    Connection ! {close_connection, Reason},
    ok.


%%% ==========================================================================
%%% Behaviour cowboy_websocket_handler Callbacks
%%% ==========================================================================

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_Transport, Req, {ProtoMod, ProtoOpts}) ->
    {ok, NewReq, Proto} = erodws_protocol:new(Req, ProtoMod, ProtoOpts),
    State = schedule_keepalive(#?St{proto = Proto}),
    {ok, cowboy_req:compact(NewReq), State, ?INACTIVITY_TIMEOUT}.


websocket_handle({pong, <<>>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, <<"\n">>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, Json}, Req, #?St{proto = Proto} = State) ->
    case erodws_protocol:handle_packet(json, Json, Proto) of
        {ok, NewProto} -> {ok, Req, State#?St{proto = NewProto}};
        {error, _Reason, NewProto} ->
            {shutdown, Req, State#?St{proto = NewProto}}
    end;

websocket_handle(Frame, Req, State) ->
    lager:error("Unexpected websocket frame: ~p", [Frame]),
    {ok, Req, State}.


websocket_info(keepalive, Req, State) ->
    {reply, ping, Req, schedule_keepalive(State)};

websocket_info({notify, _Cls, _Fmt, _Data}, Req, State) ->
    %TODO
    {ok, Req, State};

websocket_info({send_error, Cls, Err}, Req, #?St{proto = Proto} = State) ->
    {ok, Data, NewProto} = erodws_protocol:encode_error(json, Cls, Err, Proto),
    {reply, {text, Data}, Req, State#?St{proto = NewProto}};

websocket_info({send_error_reply, C, I, Err}, Req, #?St{proto = P} = State) ->
    {ok, Data, P2} = erodws_protocol:encode_error_reply(json, C, I, Err, P),
    {reply, {text, Data}, Req, State#?St{proto = P2}};

websocket_info({send_result_reply, C, I, S, Res}, Req, State) ->
    #?St{proto = P} = State,
    {ok, Data, P2} = erodws_protocol:encode_result_reply(json, C, I, S, Res, P),
    {reply, {text, Data}, Req, State#?St{proto = P2}};

websocket_info({attach_context, Ctx}, Req, #?St{proto = Proto} = State) ->
    {ok, Req, State#?St{proto = erodws_protocol:attach_context(Ctx, Proto)}};

websocket_info(release_context, Req, #?St{proto = Proto} = State) ->
    {ok, Req, State#?St{proto = erodws_protocol:release_context(Proto)}};

websocket_info({close_connection, Reason}, Req, #?St{proto = Proto} = State) ->
    {shutdown, Req, State#?St{proto = erodws_protocol:shutdown(Reason, Proto)}};

websocket_info(Info, Req, State) ->
    lager:error("Unexpected message: ~p", [Info]),
    {ok, Req, State}.


websocket_terminate(Reason, _Req, #?St{proto = Proto} = State) ->
    _ = erodws_protocol:terminated(Reason, Proto),
    _ = cancel_keepalive(State),
    ok.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

schedule_keepalive(State) ->
    State#?St{ping_timer = erlang:send_after(?PING_PERIOD, self(), keepalive)}.


cancel_keepalive(State) ->
    _ = erlang:cancel_timer(State#?St.ping_timer),
    State#?St{ping_timer = undefined}.
