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

-module(dummy_user_info).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("dummy_user.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([encode/2]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

encode(jsx, Result) ->
    #?MODULE{token = Token, ref = Ref, custom = Custom} = Result,
    [{<<"token">>, erodlib_jsx:encode_str(token, Token)},
     {<<"ref">>, erodlib_jsx:encode_key(ref, Ref)},
     {<<"custom">>, erodlib_jsx:encode_any(custom, Custom)}].

