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

-define(St, ?MODULE).
-define(USER_IDENTIY_MAP, erod_user_identity_map).


-record(?St, {}).

%%% FIXME: Removal of dead processes from the ETS table require a full scan.

start_link() ->
    gen_server:start_link({local, ?USER_MANAGER}, ?MODULE, [], []).


find_user(#?UserIdent{} = UserIdent) ->
    try ets:lookup(?USER_IDENTIY_MAP, UserIdent) of
        [] -> gen_server:call(?USER_MANAGER, {find_user, UserIdent});
        [{_, UserPid}] -> {ok, UserPid}
    catch
        eror:badarg ->
            lager:error("Tried to find a user by its identity but the ETS "
                        "table does not seem to exist"),
            {error, internal_error}
    end.


init([]) ->
    lager:info("Starting user manager...", []),
    process_flag(trap_exit, true),
    ets:new(?USER_IDENTIY_MAP, [named_table]),
    {ok, #?St{}}.


handle_call({find_user, UserIdent}, _From, State) ->
    {reply, lookup_or_start_user(UserIdent), State};

handle_call(Request, From, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info({'DOWN', _, process, Pid, _Reason}, State) ->
    lager:debug("User ~p died, removing it from the lookup table...", [Pid]),
    MatchSpec = [{{'_', '$1'}, [{'=:=', '$1', Pid}], [true]}],
    _ = ets:select_delete(?USER_IDENTIY_MAP, MatchSpec),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:debug("Terminating user manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


lookup_or_start_user(UserIdent) ->
    case ets:lookup(?USER_IDENTIY_MAP, UserIdent) of
        [{_, UserPid}] -> {ok, UserPid};
        [] -> start_new_user(UserIdent)
    end.


start_new_user(UserIdent) ->
    case erod_user_sup:start_child(UserIdent) of
        {ok, UserPid} ->
            erlang:monitor(process, UserPid),
            ets:insert(?USER_IDENTIY_MAP, {UserIdent, UserPid}),
            {ok, UserPid};
        {error, _} = Error -> Error
    end.
