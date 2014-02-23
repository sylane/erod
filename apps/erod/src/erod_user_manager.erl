-module(erod_user_manager).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([find_user/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PROCESS, ?MODULE).
-define(St, ?MODULE).
-define(USER_IDENT_TO_PID, erod_user_identity_to_pid).
-define(USER_PID_TO_IDENT, erod_user_pid_to_identity).


-record(?St, {ident_to_pid :: ets:tid(),
              pid_to_ident :: ets:tid()}).


start_link() ->
    gen_server:start_link({local, ?PROCESS}, ?MODULE, [], []).


find_user(#?UserIdent{} = UserIdent) ->
    try ets:lookup(?USER_IDENT_TO_PID, UserIdent) of
        [] -> gen_server:call(?PROCESS, {find_user, UserIdent});
        [{_, UserPid}] -> {ok, UserPid}
    catch
        % FIXME: Remove this defensive code.
        eror:badarg ->
            lager:error("Tried to find a user by its identity but the ETS "
                        "table does not seem to exist"),
            {error, internal_error}
    end.


init([]) ->
    lager:info("Starting user manager...", []),
    process_flag(trap_exit, true),
    I2P = ets:new(?USER_IDENT_TO_PID, [named_table, protected]),
    P2I = ets:new(?USER_PID_TO_IDENT, [private, {keypos, 2}]),
    {ok, #?St{ident_to_pid = I2P, pid_to_ident = P2I}}.


handle_call({find_user, UserIdent}, _From, State) ->
    case lookup_or_start_user(UserIdent, State) of
        {ok, UserPid, NewState} -> {reply, {ok, UserPid}, NewState};
        {error, Reason, NewState} -> {reply, {error, Reason}, NewState}
    end;

handle_call(Request, From, State) ->
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
    lager:debug("Terminating user manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


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
