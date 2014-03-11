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
%%% @doc TODO: Document module erod_session_manager.
%%% @end
%%% @private
%%% ==========================================================================

-module(erod_session_manager).

-author('Sebastien Merle').

-behaviour(gen_server).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control functions
-export([start_link/0]).

%%% API functions
-export([new_session/3,
         find_session/1]).

%%% Behaviour gen_serer callacks
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
-define(SESSION_TOKEN_TO_PID, erod_session_token_to_pid).
-define(SESSION_PID_TO_TOKEN, erod_session_pid_to_token).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {token_to_pid :: ets:tid(),
              pid_to_token :: ets:tid(),
              next_id = 1 :: pos_integer()}).


%%% ==========================================================================
%%% Process Control Funtions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts and links a session manager process.
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
%% @doc Creates a new session for specified user identifier, process and policy.
%% @end
%% -----------------------------------------------------------------
-spec new_session(UserId, UserPid, Policy)
    -> {ok, SessionId, SessionPid, SessionToken} | {error, Reason}
    when UserId :: erod:user_id(), UserPid :: pid(), Policy :: erod:policy(),
         SessionId :: erod:session_id(), SessionPid :: pid(),
         SessionToken :: binary(), Reason :: term().
%% -----------------------------------------------------------------

new_session(UserId, UserPid, Policy) ->
    gen_server:call(?PROCESS, {new_session, UserId, UserPid, Policy}).


%% -----------------------------------------------------------------
%% @doc Returns the session with specified token if it exists.
%% @end
%% -----------------------------------------------------------------
-spec find_session(SessionToken) -> {ok, SessionPid} | {error, Reason}
    when SessionToken :: binary(), SessionPid :: pid(),
         Reason :: session_not_found | term().
%% -----------------------------------------------------------------

find_session(Token) when is_binary(Token) ->
    case ets:lookup(?SESSION_TOKEN_TO_PID, Token) of
        [] -> {error, session_not_found};
        [{_, SessionPid}] -> {ok, SessionPid}
    end.


%%% ==========================================================================
%%% Behaviour gen_server Callbacks
%%% ==========================================================================

init([]) ->
    lager:info("Starting session manager...", []),
    process_flag(trap_exit, true),
    T2P = ets:new(?SESSION_TOKEN_TO_PID, [named_table, protected]),
    P2T = ets:new(?SESSION_PID_TO_TOKEN, [private, {keypos, 2}]),
    {ok, #?St{token_to_pid = T2P, pid_to_token = P2T}}.


handle_call({new_session, UserId, UserPid, Policy}, _From, State) ->
    case start_session(UserId, UserPid, Policy, State) of
        {error, Reason, NewState} -> {reply, {error, Reason}, NewState};
        {ok, Id, Pid, Token, NewState} ->
            {reply, {ok, Id, Pid, Token}, NewState}
    end;

handle_call(Request, {From, _Ref}, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info({'DOWN', _, process, Pid, _Reason}, State) ->
    lager:debug("Session ~p died, removing it from the lookup table...", [Pid]),
    {ok, NewState} = remove_session(Pid, State),
    {noreply, NewState};

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:info("Terminating session manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

start_session(UserId, UserPid, Policy, State) ->
    #?St{pid_to_token = P2T, token_to_pid = T2P, next_id = SessId} = State,
    {ok, Token, NewState} = find_unique_token(State),
    case erod_session_sup:start_child(SessId, UserId, UserPid, Policy, Token) of
        {error, Reason} -> {error, Reason, NewState};
        {ok, SessPid} ->
            erlang:monitor(process, SessPid),
            Item = {Token, SessPid},
            ets:insert(P2T, Item),
            ets:insert(T2P, Item),
            {ok, SessId, SessPid, Token, NewState#?St{next_id = SessId + 1}}
    end.


remove_session(SessPid, #?St{pid_to_token = P2T, token_to_pid = T2P} = State) ->
    case ets:lookup(P2T, SessPid) of
        [] -> {ok, State};
        [{Token, SessPid}] ->
            ets:delete(T2P, Token),
            ets:delete(P2T, SessPid),
            {ok, State}
    end.


find_unique_token(#?St{token_to_pid = T2P} = State) ->
    Token = base64:encode(crypto:strong_rand_bytes(16)),
    case ets:lookup(T2P, Token) of
        [] -> {ok, Token, State};
        _ -> find_unique_token(State)
    end.
