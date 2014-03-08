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
%%% @doc
%%% @end
%%% @private
%%% ==========================================================================

-module(erodws_proto_get_content_result).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include_lib("erod/include/erod_document.hrl").


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-export([encode/2]).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Encodes a get_content response to specified format.
%% @throws {format_error, Reason} for any encoding error.
%% @end
%% -----------------------------------------------------------------
-spec encode(Fmt, Response) -> Data
    when Fmt :: jsx, Response :: erod:content() | undefined, Data :: term().
%% -----------------------------------------------------------------

encode(jsx, undefined) -> undefined;

encode(jsx, #erod_content{type = entity} = Content) ->
    #erod_content{key = Key, ver = Ver, data = Data} = Content,
    [{<<"key">>, erodlib_jsx:key_value(key, Key)},
     {<<"ver">>, erodlib_jsx:ver_value(ver, Ver)},
     {<<"content">>, erodlib_jsx:struct_value(content, Data)}];

encode(jsx, #erod_content{type = patch} = Content) ->
    #erod_content{key = Key, ver = Ver, data = Data} = Content,
    [{<<"key">>, erodlib_jsx:key_value(key, Key)},
     {<<"ver">>, erodlib_jsx:ver_value(ver, Ver)},
     {<<"patch">>, erodlib_jsx:patch_value(patch, Data)}].
