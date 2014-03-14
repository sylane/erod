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

-module(erdom_user).

-author('Sebastien Merle').

-behaviour(erod_user).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include_lib("erod/include/erod_policy.hrl").

-include("erdom_storage.hrl").
-include("erdom_user.hrl").


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
    case erdom_storage:get_user_by_username(Username) of
        {error, user_not_found} -> {stop, unknown_user};
        {ok, UserData} ->
            #erdom_storage_user{id = I, username = U, password = P} = UserData,
            {ok, I, #?St{id = I, user = U, pass = P}}
    end.


info(SessionToken, #?St{id = UserId} = State) ->
    Config = [{media_server, <<"http://foo.bar.com/statics">>}],
    {ok, #erdom_proto_user_info{session = SessionToken,
                                self = {user, UserId},
                                rooms = {rooms, UserId},
                                fav = {fav, UserId},
                                root = {index, 0},
                                home = undefined,
                                config = Config}, State}.


authenticate({User, Pass}, #?St{user = User, pass = Pass} = State) ->
    {ok, #erod_policy{}, State};

authenticate({User, _}, #?St{user = User} = State) ->
    {error, invalid_credencial, State};

authenticate({User1, _}, #?St{user = User2} = State) ->
    lager:error("User ~p received an authentication request for user ~p",
                [User2, User1]),
    {error, invalid_credencial, State}.
