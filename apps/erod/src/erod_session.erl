-module(erod_session).

-behaviour(gen_fsm).

-include("erod_internal.hrl").

-export([start_link/1]).

-export([bind_context/1]).
-export([disband_context/1]).
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

%%% The time the session will wait for a client to initiate a login
%%% and for the user to bind the session before exiting.
-define(WAIT_LOGIN_TIMEOUT, 20000).

-record(?St, {token :: binary(),
              conn :: pid() | undefined,
              user :: pid() | undefined,
              policy :: erod_session_policy() | undefined,
              login_timeref :: reference() | undefined}).

%%% TODO: Add the wait_reconnect state and handle reconnection.


start_link(SessionToken) ->
    gen_fsm:start_link(?MODULE, [SessionToken], []).


bind_context(#?Ctx{sess = Session} = Ctx) ->
    gen_fsm:sync_send_event(Session, {bind_context, Ctx}).


disband_context(#?Ctx{sess = Session} = Ctx) ->
    gen_fsm:sync_send_event(Session, {disband_context, Ctx}).


route(Session, #?Msg{} = Msg, #?Ctx{} = Ctx) ->
    gen_fsm:send_event(Session, {route, Msg, Ctx}).


init([SessionToken]) ->
    lager:debug("Starting session ~p...", [SessionToken]),
    process_flag(trap_exit, true),
    TimeRef = gen_fsm:send_event_after(?WAIT_LOGIN_TIMEOUT, login_timeout),
    {ok, wait_login, #?St{token = SessionToken,
                          login_timeref = TimeRef}}.


handle_event(Event, StateName, State) ->
    lager:error("Unexpected event in state ~p: ~p", [StateName, Event]),
    {stop, {unexpected_event, StateName, Event}, State}.


handle_sync_event(Event, From, StateName, State) ->
    lager:error("Unexpected event from ~p in state ~p: ~p",
                [From, StateName, Event]),
    {stop, {unexpected_event, StateName, From, Event},
     {error, unexpected_event}, State}.


handle_info({'EXIT', Conn, _}, established, #?St{conn = Conn} = State) ->
    lager:debug("Connection ~p died, waiting for reconnection...", [Conn]),
    {next_state, wait_reconnect,
     State#?St{conn = undefined, policy = undefined}};

handle_info({'EXIT', User, _}, StateName, #?St{user = User} = State) ->
    lager:debug("User ~p died while in state ~p, committing suicide...", [User, StateName]),
    {stop, user_died,
     State#?St{conn = undefined, user = undefined, policy = undefined}};

handle_info(Info, StateName, State) ->
    lager:warning("Unexpected message in state ~p: ~p", [StateName, Info]),
    {noreply, State}.


terminate(Reason, StateName, _State) ->
    lager:debug("Terminating session in state ~p: ~p", [StateName, Reason]),
    ok.


code_change(_OldVsn, _StateName, State, _Extra) ->
    {ok, State}.


wait_login({route, Msg, Ctx}, State) ->
    lager:debug("Routing through wait_login session: ~p", [Msg]),
    case route_login(Msg, Ctx, State) of
        {stop, NewState} -> {stop, normal, NewState};
        {error, Reason, NewState} -> {stop, Reason, NewState};
        {ok, NewState} -> {next_state, wait_login, NewState}
    end;

wait_login(login_timeout, State) ->
    lager:debug("Session ~p login time exausted, terminating...", [State#?St.token]),
    {stop, normal, State};

wait_login(Event, State) ->
    lager:error("Unexpected event in state wait_login: ~p", [Event]),
    {stop, {unexpected_event, Event}, State}.


wait_login({bind_context, Ctx}, _From, State) ->
    #?Ctx{conn = Conn, user = User, policy = Policy} = Ctx,
    lager:debug("Binding session to user ~p...", [User]),
    case erod_connection:bind_context(Ctx) of
        {error, Reason} = Error-> {stop, Reason, Error, State};
        ok ->
            try erlang:link(Conn) of
                true ->
                    gen_fsm:cancel_timer(State#?St.login_timeref),
                    {reply, {ok, State#?St.token}, established,
                     State#?St{conn = Conn, user = User, policy = Policy,
                               login_timeref = undefined}}
            catch
                error:badarg ->
                    % The connection is dead without sending any token,
                    % the reconnection is impossible. R.I.P
                    Error = {internal_error, connection_died},
                    {stop, Error, {error, Error}, State}
            end
    end;

wait_login({disband_context, #?Ctx{user = User}}, _From,
           #?St{conn = undefined, user = undefined} = State) ->
    lager:debug("Session binding with user ~p canceled, terminating...", [User]),
    {stop, normal, ok, State};

wait_login(Event, From, State) ->
    lager:error("Unexpected event from ~p in state wait_login: ~p", [From, Event]),
    {stop, {unexpected_event, Event, From}, {error, unexpected_event}, State}.


established({route, Msg, #?Ctx{conn = Conn, sess = Sess, user = User} = Ctx},
            #?St{conn = Conn, user = User} = State) when Sess =:= self() ->
    lager:debug("Routing through established session: ~p", [Msg]),
    case route_established(Msg, Ctx, State) of
        {stop, NewState} -> {stop, normal, NewState};
        {error, Reason, NewState} -> {stop, Reason, NewState};
        {ok, NewState} -> {next_state, established, NewState}
    end;

established({route, Msg, Ctx}, State) ->
    erod_context:discard(Ctx, Msg),
    {ok, State};

established(Event, State) ->
    lager:error("Unexpected event in state established: ~p", [Event]),
    {stop, {unexpected_event, Event}, State}.


established({bind_context, _Ctx}, _From, State) ->
    % Use and internal_error because this is NOT supposed to happen
    Error = {internal_error, session_already_connected},
    {reply, {error, Error}, established, State};

established({disband_context, #?Ctx{conn = Conn, user = User} = Ctx}, _From,
            #?St{conn = Conn, user = User} = State) ->
    lager:debug("Detaching session from user ~p...", [User]),
    erod_connection:disband_context(Ctx),
    catch erlang:unlink(Conn),
    {stop, normal, ok,
     State#?St{conn = undefined, user = undefined, policy = undefined}};

established(Event, From, State) ->
    lager:error("Unexpected event from ~p in state established: ~p", [From, Event]),
    {stop, {unexpected_event, Event, From}, {error, unexpected_event}, State}.



route_login(#?Msg{type = request, cls = login} = Req, Ctx, State) ->
    case erod_context:safe_decode_data(Ctx, Req, ?MsgLogReq) of
        {error, _Reason} -> {ok, State};
        {ok, #?Msg{data = LogReq} = NewReq} ->
            UserIdent = ?MsgLogReq:as_identity(LogReq),
            case erod_user_manager:find_user(UserIdent) of
                {ok, User} ->
                    erod_user:route(User, NewReq, Ctx#?Ctx{sess = self()}),
                    {ok, State};
                {error, Reason} ->
                    Error = {login_error, Reason},
                    erod_context:reply_error(Ctx, NewReq, Error),
                    {stop, State}
            end
    end;

route_login(Msg, Ctx, State) ->
    erod_context:discard(Ctx, Msg),
    {ok, State}.


route_established(#?Msg{type = request, cls = logout} = Req, Ctx, State) ->
    erod_user:route(State#?St.user, Req, Ctx),
    {ok, State};

route_established(Msg, Ctx, State) ->
    erod_context:discard(Ctx, Msg),
    {ok, State}.
