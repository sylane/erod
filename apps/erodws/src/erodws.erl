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
%%% @doc TODO: Document module erodws.
%%% @end
%%% ==========================================================================

-module(erodws).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([start/1,
         start/3]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts the websocket acceptors on specified port.
%% @end
%% -----------------------------------------------------------------
-spec start(Port) -> {ok, Pid} | {error, Reason}
    when Port :: pos_integer(), Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start(Port) ->
    start(Port, erodws_protocol_default, []).


%% -----------------------------------------------------------------
%% @doc Starts the websocket acceptors on specified port
%% with a custom protocol behaviour.
%% @end
%% -----------------------------------------------------------------
-spec start(Port, Protocol, Options) -> {ok, Pid} | {error, Reason}
    when Port :: pos_integer(), Protocol :: module(), Options :: term(),
         Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start(Port, Protocol, Options) when is_integer(Port) ->
    Dispatch = cowboy_router:compile(
                 [ {'_', [{'_', erodws_connection, {Protocol, Options}}]} ]),
    cowboy:start_http(http_listener, 100, [{port, 8888}],
                      [{env, [{dispatch, Dispatch}]}]).
