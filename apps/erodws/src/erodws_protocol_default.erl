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

-module(erodws_protocol_default).

-author('Sebastien Merle').

-behaviour(erodws_protocol).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

%%% Behaviour erodws_protocol callacks
-export([init/2,
         decode_login_credential/3,
         decode_restore_credential/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {}).


%%% ==========================================================================
%%% Behaviour erodws_protocol Callbacks
%%% ==========================================================================

init(Req, []) ->
    {ok, Req, #?St{}}.


decode_login_credential(Fmt, Data, State) ->
    {Ident, Cred} = erodws_proto_login_credential:decode(Fmt, Data),
    {ok, Ident, Cred, State}.


decode_restore_credential(Fmt, Data, State) ->
    {Ident, Token} = erodws_proto_restore_credential:decode(Fmt, Data),
    {ok, Ident, Token, State}.



