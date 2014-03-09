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
%%% @doc TODO: Document module erodws_errors.
%%% @end
%%% @private
%%% ==========================================================================

-module(erodws_errors).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erodws_protocol.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([map/2]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(Err, erodws_proto_generic_error).
-define(ERROR(Status, Code, Msg, Debug),
        #?Err{status = Status, code = Code,
              msg = Msg, debug = format_debug(Debug)}).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Maps an erlang internal error to an error record.
%% @end
%% -----------------------------------------------------------------
-spec map(Cls, Error) -> erodws_error()
    when Cls :: atom(), Info :: term(),
         Error :: atom() | {atom(), Info} | {atom(), {atom(), Info}}.
%% -----------------------------------------------------------------

map(Cls, A) when is_atom(A) ->
    map(Cls, {A, {undefined, undefined}});

map(Cls, {A, B}) when is_atom(A), is_atom(B) ->
    map(Cls, {A, {B, undefined}});

map(login, {format_error, Debug}) ->
    ?ERROR(400, 4000100, <<"Message Format Error">>, Debug);

map(login, {already_authenticated, Debug}) ->
    ?ERROR(401, 4010110, <<"Already Authenticated">>, Debug);

map(login, {unknown_user, Debug}) ->
    ?ERROR(401, 4010120, <<"User Not Found">>, Debug);

map(login, {invalid_credencial, Debug}) ->
    ?ERROR(401, 4010130, <<"Invalid Credential">>, Debug);

map(login, Debug) ->
    ?ERROR(500, 5000199, <<"Login Error">>, Debug);

map(reconnect, {format_error, Debug}) ->
    ?ERROR(400, 4000200, <<"Message Format Error">>, Debug);

map(reconnect, {already_authenticated, Debug}) ->
    ?ERROR(401, 4010210, <<"Already Authenticated">>, Debug);

map(reconnect, {session_not_found, Debug}) ->
    ?ERROR(401, 4010220, <<"Unknown Session">>, Debug);

map(reconnect, Debug) ->
    ?ERROR(500, 5000299, <<"Reconnect Error">>, Debug);

map(logout, {format_error, Debug}) ->
    ?ERROR(400, 4000300, <<"Message Format Error">>, Debug);

map(logout, {not_authenticated, Debug}) ->
    ?ERROR(401, 4010310, <<"Not Authenticated">>, Debug);

map(logout, Debug) ->
    ?ERROR(500, 5000399, <<"Logout Error">>, Debug);

map(get_content, {format_error, Debug}) ->
    ?ERROR(400, 4000400, <<"Message Format Error">>, Debug);

map(get_content, {not_authenticated, Debug}) ->
    ?ERROR(401, 4010410, <<"Not Authenticated">>, Debug);

map(get_content, {document_not_found, Debug}) ->
    ?ERROR(404, 4040420, <<"Document Not Found">>, Debug);

map(get_content, Debug) ->
    ?ERROR(500, 5000499, <<"Content Retrieval Error">>, Debug);

map(get_children, {format_error, Debug}) ->
    ?ERROR(400, 4000500, <<"Message Format Error">>, Debug);

map(get_children, {not_authenticated, Debug}) ->
    ?ERROR(401, 4010510, <<"Not Authenticated">>, Debug);

map(get_children, {document_not_found, Debug}) ->
    ?ERROR(404, 4040520, <<"Document Not Found">>, Debug);

map(get_children, {view_not_found, Debug}) ->
    ?ERROR(404, 4040521, <<"View Not Found">>, Debug);

map(get_children, {page_not_found, Debug}) ->
    ?ERROR(404, 4040522, <<"Page Not Found">>, Debug);

map(get_children, Debug) ->
    ?ERROR(500, 5000599, <<"Children Retrieval Error">>, Debug);

map(patch_content, {format_error, Debug}) ->
    ?ERROR(400, 4000600, <<"Message Format Error">>, Debug);

map(patch_content, {not_authenticated, Debug}) ->
    ?ERROR(401, 4010610, <<"Not Authenticated">>, Debug);

map(patch_content, {document_not_found, Debug}) ->
    ?ERROR(404, 4040620, <<"Document Not Found">>, Debug);

map(patch_content, {conflict, Debug}) ->
    ?ERROR(409, 4090630, <<"Version Conflict">>, Debug);

map(patch_content, Debug) ->
    ?ERROR(500, 5000699, <<"Content Retrieval Error">>, Debug);

map(_, {format_error, Debug}) ->
    ?ERROR(400, 4000000, <<"Message Format Error">>, Debug);

map(_, {internal_error, Debug}) ->
    ?ERROR(500, 5000000, <<"Internal Server Error">>, Debug);

map(_, Debug) ->
    ?ERROR(500, 5000000, <<"Internal Server Error">>, Debug).


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

format_debug(undefined) -> undefined;

format_debug({undefined, undefined}) -> undefined;

format_debug({Something, undefined}) -> Something;

format_debug({Something, {undefined, undefined}}) -> Something;

format_debug({Some, {Thing, undefined}}) -> {Some, Thing};

format_debug(Something) -> Something.
