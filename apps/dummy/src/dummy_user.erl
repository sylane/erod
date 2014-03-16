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

-module(dummy_user).

-author('Sebastien Merle').

-behaviour(erod_user).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include_lib("erod/include/erod_policy.hrl").

-include("dummy_user.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Behaviour erod_user callbacks
-export([init/2,
         info/2,
         authenticate/2]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).

%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {id :: pos_integer(),
              user :: binary(),
              pass :: binary()}).


%%% ==========================================================================
%%% Behaviour erod_user Callbacks
%%% ==========================================================================

init(Username, []) ->
	{ok, #?St{id = 1, user = Username, pass = <<"">>}}.


info(SessionToken, State) ->
    Custom = [{key, <<"Some\nValue">>}],
    {ok, #dummy_user_info{token = SessionToken,
                          ref = {dummy, 1},
                          custom = Custom}, State}.


authenticate({User, Pass}, #?St{user = User, pass = Pass} = State) ->
    {ok, #erod_policy{}, State};

authenticate({User, _}, #?St{user = User} = State) ->
    {error, invalid_credencial, State};

authenticate({_User1, _}, #?St{user = _User2} = State) ->
    {error, invalid_credencial, State}.
