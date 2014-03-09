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
%%% @doc TODO: Document module erodws_proxy.
%%% @end
%%% @private
%%% ==========================================================================

-module(erodws_proxy).

-author('Sebastien Merle').

-behaviour(erod_proxy).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

%%% API functions
-export([new/1]).

%%% Behaviour erod_proxy callbacks
-export([init/1,
         accept/3,
         handle_message/3,
         notify/5]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {conn}).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new proy for a connection.
%% @end
%% -----------------------------------------------------------------
-spec new(Connection) -> Proxy
    when Connection :: pid(), Proxy :: erod:proxy().
%% -----------------------------------------------------------------

new(Connection) ->
    erod_proxy:new(?MODULE, [Connection]).


%%% ==========================================================================
%%% behaviour erod_proy Callbacks
%%% ==========================================================================

init([Conn]) ->
     #?St{conn = Conn}.


accept(_Ctx, _Proxy, #?St{conn = Conn} = State) ->
    try erlang:link(Conn) of
        true -> {ok, State}
    catch
        error:badarg -> {error, link_failed, State}
    end.


handle_message({'EXIT', Conn, Reason}, _Proxy, #?St{conn = Conn} = State) ->
    {dead, Reason, State};

handle_message(_, _Proxy, _State) -> ignored.


notify(Name, Fmt, Data, _Proxy, #?St{conn = Conn} = State) ->
    erodws_connection:notify(Conn, Name, Fmt, Data),
    State.
