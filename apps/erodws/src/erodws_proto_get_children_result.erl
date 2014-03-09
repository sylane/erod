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
%%% @doc TODO: Document module erodws_proto_get_children_result.
%%% @end
%%% @private
%%% ==========================================================================

-module(erodws_proto_get_children_result).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include_lib("erod/include/erod_document.hrl").


%%% ==========================================================================
%%% Eports
%%% ==========================================================================

%%% API functions
-export([encode/2]).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Encodes a get_children response to specified format.
%% @throws {format_error, Reason} for any encoding error.
%% @end
%% -----------------------------------------------------------------
-spec encode(Fmt, Response) -> Data
    when Fmt :: jsx, Response :: #erod_page{} | undefined, Data :: term().
%% -----------------------------------------------------------------

encode(jsx, undefined) -> undefined;

encode(jsx, #erod_page{type = entity} = Page) ->
    #erod_page{key = Key, ver = Ver, view = ViewId, page = PageId,
               size = PageSize, total = TotalSize, data = Data} = Page,
    [{<<"key">>, erodlib_jsx:encode_key(key, Key)},
     {<<"ver">>, erodlib_jsx:encode_ver(ver, Ver)},
     {<<"view">>, erodlib_jsx:encode_str(view, ViewId)},
     {<<"page">>, erodlib_jsx:encode_int(page, PageId)},
     {<<"size">>, erodlib_jsx:encode_int(size, PageSize)},
     {<<"total">>, erodlib_jsx:encode_int(total, TotalSize)},
     {<<"page">>, [[{<<"key">>, erodlib_jsx:encode_key(key, K)}
                    |erodlib_jsx:encode_any(page, V)]
                   || {K, V} <- Data]}];

encode(jsx, #erod_page{type = patch} = Page) ->
    #erod_page{key = Key, ver = Ver, view = ViewId, page = PageId,
               size = PageSize, total = TotalSize, data = Data} = Page,
    [{<<"key">>, erodlib_jsx:encode_key(key, Key)},
     {<<"ver">>, erodlib_jsx:encode_ver(ver, Ver)},
     {<<"view">>, erodlib_jsx:encode_str(view, ViewId)},
     {<<"page">>, erodlib_jsx:encode_int(page, PageId)},
     {<<"size">>, erodlib_jsx:encode_int(size, PageSize)},
     {<<"total">>, erodlib_jsx:encode_int(total, TotalSize)},
     {<<"patch">>, erodlib_jsx:encode_patch(page, Data)}].
