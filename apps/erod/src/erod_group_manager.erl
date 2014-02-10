-module(erod_group_manager).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(St, ?MODULE).

-record(?St, {}).


start_link() ->
    gen_server:start_link({local, ?GROUP_MANAGER}, ?MODULE, [], []).


init([]) ->
    lager:info("Starting group manager...", []),
    process_flag(trap_exit, true),
    {ok, #?St{}}.


handle_call(Request, From, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:debug("Terminating goup manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
