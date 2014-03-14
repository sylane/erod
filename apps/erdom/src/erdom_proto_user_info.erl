%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erdom.
%%%
%%% Erdom is free software: you can redistribute it and/or modify
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
%%% @doc TODO: Document module erod_actions.
%%% @end
%%% ==========================================================================

-module(erdom_proto_user_info).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erdom_user.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([encode/2]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Encodes user's information records to specified format.
%% @end
%% -----------------------------------------------------------------
-spec encode(Format, Value) -> Data
    when Format :: jsx, Value :: #?MODULE{}, Data :: term().
%% -----------------------------------------------------------------

encode(jsx, Result) ->
    #?MODULE{session = Session, self = Self, rooms = Rooms, fav = Fav,
             root = Root, home = Home, config = Config} = Result,
    [{<<"session">>, erodlib_jsx:encode_str(session, Session)},
     {<<"self">>, erodlib_jsx:encode_key(self, Self)},
     {<<"rooms">>, erodlib_jsx:encode_key(rooms, Rooms)},
     {<<"fav">>, erodlib_jsx:encode_key(fav, Fav)},
     {<<"root">>, erodlib_jsx:encode_key(root, Root)},
     {<<"home">>, erodlib_jsx:encode_key(home, Home)},
     {<<"config">>, erodlib_jsx:encode_any(config, Config)}].

