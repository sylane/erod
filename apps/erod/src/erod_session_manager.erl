-module(erod_session_manager).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([route/2]).

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


route(Msg, Ctx) ->
    gen_server:cast(?SESSION_MANAGER, {route, Msg, Ctx}).


init([]) ->
    lager:info("Starting session manager...", []),
    process_flag(trap_exit, true),
    {ok, #?St{}}.


handle_call(Request, From, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast({route, Msg, Ctx}, State) ->
    {noreply, route(Msg, Ctx, State)};

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


route(#?Msg{type = request, cls = login} = Req, Ctx, State) ->
    case erod_context:safe_decode_data(Ctx, Req, ?MsgLogReq) of
        {error, _} -> State;
        {ok, NewReq} ->
            {ok, Session} = erod_session_sup:start_child(),
            erod_session:route(Session, NewReq, Ctx),
            State
     end;

route(#?Msg{type = request, cls = reconnect} = Req, Ctx, State) ->
    erod_context:reply_error(Ctx, Req, not_implemented),
    State;

route(Msg, Ctx, State) ->
    erod_context:discard(Ctx, Msg),
    State.
