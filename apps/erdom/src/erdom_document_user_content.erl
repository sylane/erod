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

-module(erdom_document_user_content).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erdom_document.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([encode/2]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Encodes user's content records to specified format.
%% @end
%% -----------------------------------------------------------------
-spec encode(Format, Value) -> Data
    when Format :: jsx, Value :: #?MODULE{}, Data :: term().
%% -----------------------------------------------------------------

encode(jsx, Content) ->
    #?MODULE{first_name = FN, last_name = LN, display_name = DN,
             picture = Pic, presence = Pres, connected = Conn} = Content,
    [{<<"first_name">>, erodlib_jsx:encode_str(first_name, FN)},
     {<<"last_name">>, erodlib_jsx:encode_str(last_name, LN)},
     {<<"display_name">>, erodlib_jsx:encode_str(display_name, DN)},
     {<<"picture">>, erodlib_jsx:encode_str(picture, Pic)},
     {<<"presence">>, erodlib_jsx:encode_str(presence, Pres)},
     {<<"connected">>, erodlib_jsx:encode_bool(connected, Conn)}].
