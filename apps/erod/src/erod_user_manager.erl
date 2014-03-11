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
%%% @doc TODO: Document module erod_user_manager.
%%% @end
%%% @private
%%% ==========================================================================

-module(erod_user_manager).

-author('Sebastien Merle').

-behaviour(gen_server).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control functions
-export([start_link/0]).

%%% API functions
-export([get_user/1]).

%%% Behaviour gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(PROCESS, ?MODULE).
-define(St, ?MODULE).
-define(USER_IDENT_TO_PID, erod_user_identity_to_pid).
-define(USER_PID_TO_IDENT, erod_user_pid_to_identity).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {ident_to_pid :: ets:tid(),
              pid_to_ident :: ets:tid()}).


%%% ==========================================================================
%%% Process Control Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts and links a user manager process.
%% @end
%% -----------------------------------------------------------------
-spec start_link() -> {ok, Pid} | {error, Reason}
    when Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?PROCESS}, ?MODULE, [], []).



%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Gives the uer process for given identity if it is valide.
%% If the user process for this identity is already running it is returned
%% and if not it will try to start a new user process.
%% @end
%% -----------------------------------------------------------------
-spec get_user(UserIdentity) -> {ok, UserPid} | {error, Reason}
    when UserIdentity :: term(), UserPid :: pid(),
         Reason :: unknown_user | term().
%% -----------------------------------------------------------------

get_user(UserIdent) ->
    case ets:lookup(?USER_IDENT_TO_PID, UserIdent) of
        [] -> gen_server:call(?PROCESS, {get_user, UserIdent});
        [{_, UserPid}] -> {ok, UserPid}
    end.



%%% ==========================================================================
%%% Behaviour gen_server Callbacks
%%% ==========================================================================

init([]) ->
    lager:info("Starting user manager...", []),
    process_flag(trap_exit, true),
    I2P = ets:new(?USER_IDENT_TO_PID, [named_table, protected]),
    P2I = ets:new(?USER_PID_TO_IDENT, [private, {keypos, 2}]),
    {ok, #?St{ident_to_pid = I2P, pid_to_ident = P2I}}.


handle_call({get_user, UserIdent}, _From, State) ->
    case lookup_or_start_user(UserIdent, State) of
        {ok, UserPid, NewState} -> {reply, {ok, UserPid}, NewState};
        {error, Reason, NewState} -> {reply, {error, Reason}, NewState}
    end;

handle_call(Request, {From, _Ref}, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info({'DOWN', _, process, Pid, _Reason}, State) ->
    lager:debug("User ~p died, removing it from the lookup table...", [Pid]),
    {ok, NewState} = remove_user(Pid, State),
    {noreply, NewState};

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:info("Terminating user manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

lookup_or_start_user(UserIdent, #?St{ident_to_pid = I2P} = State) ->
    case ets:lookup(I2P, UserIdent) of
        [{_, UserPid}] -> {ok, UserPid, State};
        [] -> start_user(UserIdent, State)
    end.


start_user(UserIdent, #?St{pid_to_ident = P2I, ident_to_pid = I2P} = State) ->
    case erod_user_sup:start_child(UserIdent) of
        {error, Reason} -> {error, Reason, State};
        {ok, UserPid} ->
            erlang:monitor(process, UserPid),
            Item = {UserIdent, UserPid},
            ets:insert(P2I, Item),
            ets:insert(I2P, Item),
            {ok, UserPid, State}
    end.


remove_user(UserPid, #?St{pid_to_ident = P2I, ident_to_pid = I2P} = State) ->
    case ets:lookup(P2I, UserPid) of
        [] -> {ok, State};
        [{Ident, UserPid}] ->
            ets:delete(I2P, Ident),
            ets:delete(P2I, UserPid),
            {ok, State}
    end.
