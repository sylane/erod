-module(erod_session).

-behaviour(gen_fsm).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([route/3]).

-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

-export([wait_login/2, wait_login/3,
         established/2, established/3]).


-define(St, ?MODULE).

-record(?St, {username :: binary() | undefined}).


start_link() ->
    gen_fsm:start_link(?MODULE, [], []).


route(Session, #?Msg{} = Msg, #?Ctx{} = Ctx) ->
    gen_fsm:send_event(Session, {route, Msg, Ctx}).


init([]) ->
    lager:debug("Starting session...", []),
    process_flag(trap_exit, true),
    {ok, wait_login, #?St{}}.


handle_event(Event, _StateName, State) ->
    lager:error("Unexpected event: ~p", [Event]),
    {stop, {unexpected_event, Event}, State}.


handle_sync_event(Event, From, _StateName, State) ->
    lager:error("Unexpected event from ~p: ~p", [From, Event]),
    {stop, {unexpected_event, Event, From}, {error, unexpected_event}, State}.


handle_info(Info, _StateName, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _StateName, _State) ->
    lager:debug("Terminating session: ~p", [Reason]),
    ok.


code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.


wait_login({route, Msg, Ctx}, State) ->
    case route_login(Msg, Ctx, State) of
        {stop, NewState} -> {stop, normal, NewState};
        {error, Reason, NewState} -> {stop, Reason, NewState};
        {NewStateName, NewState} -> {next_state, NewStateName, NewState}
    end;

wait_login(Event, State) ->
    lager:error("Unexpected event: ~p", [Event]),
    {stop, {unexpected_event, Event}, State}.


wait_login(Event, From, State) ->
    lager:error("Unexpected event from ~p: ~p", [From, Event]),
    {stop, {unexpected_event, Event, From}, {error, unexpected_event}, State}.


established({route, Msg, Ctx}, State) ->
    case route_established(Msg, Ctx, State) of
        {stop, NewState} -> {stop, normal, NewState};
        {error, Reason, NewState} -> {stop, Reason, NewState};
        {NewStateName, NewState} -> {next_state, NewStateName, NewState}
    end;

established(Event, State) ->
    lager:error("Unexpected event: ~p", [Event]),
    {stop, {unexpected_event, Event}, State}.


established(Event, From, State) ->
    lager:error("Unexpected event from ~p: ~p", [From, Event]),
    {stop, {unexpected_event, Event, From}, {error, unexpected_event}, State}.



route_login(#?Msg{type = request, cls = login} = Req, Ctx, State) ->
    case erod_context:safe_decode_data(Ctx, Req, ?MsgLogReq) of
        {error, Reason} -> {error, Reason, State};
        {ok, #?Msg{data = LogReq} = Req} ->
            case do_login(LogReq, State) of
                {ok, LogRes, NewState} ->
                    case erod_context:link_session(Ctx, self()) of
                        ok ->
                            erod_context:reply(Ctx, Req, LogRes),
                            {established, NewState};
                        _ ->
                            erod_context:reply_error(Ctx, Req, login_error),
                            % TODO: ensure the session is cleaned up
                            {error, link_error, NewState}
                    end;
                {error, Reason, NewState} ->
                    erod_context:reply_error(Ctx, Req, Reason),
                    {stop, NewState}
            end
    end;

route_login(Msg, Ctx, State) ->
    erod_context:discard(Ctx, Msg),
    {ok, State}.


route_established(#?Msg{type = request, cls = logout}, Ctx, State) ->
    lager:debug("User ~p logging out..", [State#?St.username]),
    erod_context:unlink_session(Ctx, self()),
    {stop, State};

route_established(Msg, Ctx, State) ->
    erod_context:discard(Ctx, Msg),
    {ok, State}.


do_login(LogReq, State) ->
    lager:debug("User ~p logging in..", [LogReq#?MsgLogReq.username]),
    LogRes = #?MsgLogRes{session = <<"duMySess10n">>,
                         self = #?MsgKey{type = user, id = 123123},
                         rooms = #?MsgKey{type = rooms, id = 439323},
                         fav = #?MsgKey{type = fav, id = 98734},
                         root = #?MsgKey{type = root, id = 38321},
                         home = #?MsgKey{type = unit, id = 64324},
                         config = [{media_server, <<"http://foo.bar.com/statics">>}]},
    {ok, LogRes, State#?St{username = LogReq#?MsgLogReq.username}}.
