%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erod.
%%%
%%% Erod is free software: you can redistribute it and/or modify
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
%%% @doc TODO: Document module erod_user_sup.
%%% @end
%%% @private
%%% ==========================================================================

-module(erod_user_sup).

-author('Sebastien Merle').

-behaviour(supervisor).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control functions
-export([start_link/0]).

%%% API functions
-export([start_child/1]).

%% Behaviour supervisor callbacks
-export([init/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(PROCESS, ?MODULE).



%%% ==========================================================================
%%% Process Control Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts and links the user supervisor process
%% @end
%% -----------------------------------------------------------------
-spec start_link() -> {ok, Pid} | {error, Reason}
    when Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?PROCESS}, ?MODULE, []).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts a user process with specified identity.
%% @end
%% -----------------------------------------------------------------
-spec start_child(UserIdentity) -> {ok, UserPid} | {error, Reason}
    when UserIdentity :: term(), UserPid :: pid(),
         Reason :: unknown_user | term().
%% -----------------------------------------------------------------

start_child(UserIdent) ->
    supervisor:start_child(?PROCESS, [UserIdent]).


%%% ==========================================================================
%%% Behaiour supervisor Functions
%%% ==========================================================================

init([]) ->
    lager:info("Starting user supervisor...", []),
    {ok, { {simple_one_for_one, 5, 10},
          [{erod_user, {erod_user, start_link, []},
            transient, 5000, worker, [erod_user]}]} }.

