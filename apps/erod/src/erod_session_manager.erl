-module(erod_session_manager).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([new_session/0]).
-export([find_session/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(St, ?MODULE).
-define(SESSION_TOKEN_TO_PID, erod_session_token_to_pid).
-define(SESSION_PID_TO_TOKEN, erod_session_pid_to_token).

-record(?St, {token_to_pid :: ets:tid(),
              pid_to_token :: ets:tid()}).


start_link() ->
    gen_server:start_link({local, ?SESSION_MANAGER}, ?MODULE, [], []).


new_session() ->
    gen_server:call(?SESSION_MANAGER, new_session).


find_session(Token) when is_binary(Token) ->
    try ets:lookup(?SESSION_TOKEN_TO_PID, Token) of
        [] -> {error, session_not_found};
        [{_, SessionPid}] -> {ok, SessionPid}
    catch
        % FIXME: Remove this defensive code.
        eror:badarg ->
            lager:error("Tried to find a session by its token but the ETS "
                        "table does not seem to exist yet"),
            {error, internal_error}
    end.


init([]) ->
    lager:info("Starting session manager...", []),
    process_flag(trap_exit, true),
    T2P = ets:new(?SESSION_TOKEN_TO_PID, [named_table, protected]),
    P2T = ets:new(?SESSION_PID_TO_TOKEN, [private, {keypos, 2}]),
    {ok, #?St{token_to_pid = T2P, pid_to_token = P2T}}.


handle_call(new_session, _From, State) ->
    case start_session(State) of
        {ok, Sess, NewState} -> {reply, {ok, Sess}, NewState};
        {error, Reason, NewState} -> {reply, {error, Reason}, NewState}
    end;

handle_call(Request, From, State) ->
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
    lager:debug("Terminating session manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



start_session(#?St{pid_to_token = P2T, token_to_pid = T2P} = State) ->
    {ok, Token, NewState} = find_unique_token(State),
    case erod_session_sup:start_child(Token) of
        {error, Reason} -> {error, Reason, NewState};
        {ok, SessionPid} ->
            erlang:monitor(process, SessionPid),
            Item = {Token, SessionPid},
            ets:insert(P2T, Item),
            ets:insert(T2P, Item),
            {ok, SessionPid, NewState}
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
