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

-module(dummy_client).

-author('Sebastien Merle').

-behaviour(websocket_client_handler).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control functions
-export([start_link/2]).

%%% API functions
-export([send/2,
         close/1]).

%%% Behaiour websocket_client_handler callbacks
-export([init/2,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {pid :: pid()}).


%%% ==========================================================================
%%% Process Control Funtions
%%% ==========================================================================

start_link(URI, Pid) ->
	websocket_client:start_link(URI, ?MODULE, [Pid]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

send(WebSocket, Msg) when is_binary(Msg) ->
	WebSocket ! {send, Msg};

send(WebSocket, Msg) ->
	WebSocket ! {send, erodlib_jsx:encode(json, Msg)}.


close(WebSocket) ->
	WebSocket ! close.


%%% ==========================================================================
%%% Behaviour websocket_client_handler Callacks
%%% ==========================================================================

init([Pid], _ConnState) ->
    {ok, #?St{pid = Pid}}.


websocket_handle({pong, _}, _ConnState, State) ->
    {ok, State};

websocket_handle({ping, _}, _ConnState, State) ->
    ct:pal("Responding to ping request."),
    {reply, pong, State};

websocket_handle({text, Msg}, _ConnState, #?St{pid = Pid} = State) ->
    ct:pal("Received message: ~p~n", [Msg]),
    Data = try erodlib_jsx:decode(json, Msg)
           catch {format_error, _} -> Msg
		   end,
    Pid ! {received, Data},
    {ok, State}.


websocket_info(close, _ConnState, State) ->
	{close, <<>>, State};

websocket_info({send, Msg}, _ConnState, State) ->
    {reply, {text, Msg}, State};

websocket_info(Info, _ConnState, State) ->
    ct:pal("Unexpected message: ~p~n", [Info]),
    {close, <<>>, State}.


websocket_terminate({close, Code, Payload}, _ConnState, #?St{pid = Pid}) ->
    ct:pal("Websocket closed with code ~p and payload ~p~n", [Code, Payload]),
    Pid ! closed,
    ok.