-module(erod_session_manager).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([new_session/0]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(St, ?MODULE).

-record(?St, {sessions = gb_trees:empty()}).


start_link() ->
    gen_server:start_link({local, ?SESSION_MANAGER}, ?MODULE, [], []).


new_session() ->
    gen_server:call(?SESSION_MANAGER, new_session).


init([]) ->
    lager:info("Starting session manager...", []),
    process_flag(trap_exit, true),
    {ok, #?St{}}.


handle_call(new_session, _From, State) ->
    Token = base64:encode(crypto:strong_rand_bytes(32)),
    {reply, erod_session_sup:start_child(Token), State};

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
    lager:debug("Terminating session manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
