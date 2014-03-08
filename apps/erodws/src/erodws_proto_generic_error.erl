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

-module(erodws_proto_generic_error).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erodws_protocol.hrl").


%%% ==========================================================================
%%% Eports
%%% ==========================================================================

%%% API functions
-export([decode/2,
         encode/2]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Decodes an error record from specified data in specified format.
%% @throws {format_error, Reason} for any decoding error.
%% @end
%% -----------------------------------------------------------------
-spec decode(Fmt, Data) -> Error
    when Fmt :: term, Data :: term(), Error :: erodws_error().
%% -----------------------------------------------------------------

decode(term, Term) ->
    Code = erodlib_term:get_int(code, Term),
    Msg = erodlib_term:get_bin(msg, Term, undefined),
    Debug = erodlib_term:get_bin(debug, Term, undefined),
    #?MODULE{code = Code, msg = Msg, debug = Debug}.


%% -----------------------------------------------------------------
%% @doc Encodes an error record to specified format.
%% @throws {format_error, Reason} for any encoding error.
%% @end
%% -----------------------------------------------------------------
-spec encode(Fmt, Error) -> Data
    when Fmt :: jsx, Error :: erodws_error(), Data :: term().
%% -----------------------------------------------------------------

encode(jsx, #?MODULE{code = Code, msg = undefined, debug = undefined}) ->
    [{<<"code">>, erodlib_jsx:encode_int(code, Code)}];

encode(jsx, #?MODULE{code = Code, msg = Msg, debug = undefined}) ->
    [{<<"code">>, erodlib_jsx:encode_int(code, Code)},
     {<<"msg">>, erodlib_jsx:encode_str(code, Msg)}];

encode(jsx, #?MODULE{code = Code, msg = undefined, debug = Debug}) ->
    DebugData = erlang:iolist_to_binary(io_lib:format("~p", [Debug])),
    [{<<"code">>, erodlib_jsx:encode_int(code, Code)},
     {<<"debug">>, erodlib_jsx:encode_str(debug, DebugData)}];

encode(jsx, #?MODULE{code = Code, msg = Msg, debug = Debug}) ->
    DebugData = erlang:iolist_to_binary(io_lib:format("~p", [Debug])),
    [{<<"code">>, erodlib_jsx:encode_int(code, Code)},
     {<<"msg">>, erodlib_jsx:encode_str(code, Msg)},
     {<<"debug">>, erodlib_jsx:encode_str(debug, DebugData)}].

