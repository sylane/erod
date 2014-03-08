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

-module(erodws_proto_get_children_request).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erodws_protocol.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([decode/2]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Decodes a get_children request from specified data in specified format.
%% @throws {format_error, Reason} for any decoding error.
%% @end
%% -----------------------------------------------------------------
-spec decode(Fmt, Data) -> Request
    when Fmt :: term, Data :: term(), Request :: #?MODULE{}.
%% -----------------------------------------------------------------

decode(term, Term) ->
    Key = erodlib_term:get_key(key, Term),
    Ver = erodlib_term:get_ver(ver, Term, undefined),
    ViewId = erodlib_term:get_atom(view, Term),
    PageId = erodlib_term:get_integer(page, Term),
    Subscribe = erodlib_term:get_bool(subscribe, Term, false),
    #?MODULE{key = Key, ver = Ver, view = ViewId,
             page = PageId, subscribe = Subscribe}.
