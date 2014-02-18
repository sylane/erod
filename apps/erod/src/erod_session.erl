%%% ==========================================================================
%%% @doc Session.
%%% @end
%%% @private
%%% ==========================================================================
-module(erod_session).

-behaviour(gen_fsm).

%%% FIXME: handle the case of reconnection when already connected.

%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_internal.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Start/Stop functions
-export([start_link/1]).

%%% API functions
-export([bind_context/1]).
-export([disband_context/1]).
-export([route/3]).

%%% Behaviour gen_fsm standard callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%%% Behaviout gen_fsm states callbacks
-export([wait_login/2, wait_login/3,
         established/2, established/3,
         wait_reconnect/2, wait_reconnect/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).

%%% The time the session will wait for a client to login.
-define(WAIT_LOGIN_TIMEOUT, 10000).

%%% The time the session will wait for a client to reconnect.
-define(WAIT_RECONNECT_TIMEOUT, 30*60*1000).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {token :: binary(),
              conn :: pid() | undefined,
              user :: pid() | undefined,
              policy :: erod_session_policy() | undefined,
              login_timeref :: reference() | undefined,
              reconnect_timeref :: reference() | undefined}).


%%% ==========================================================================
%%% Start/Stop Functions
%%% ==========================================================================

start_link(SessionToken) ->
    gen_fsm:start_link(?MODULE, [SessionToken], []).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

bind_context(#?Ctx{sess = Session} = Ctx) ->
    gen_fsm:sync_send_event(Session, {bind_context, Ctx}).


disband_context(#?Ctx{sess = Session} = Ctx) ->
    gen_fsm:sync_send_event(Session, {disband_context, Ctx}).


route(Session, #?Msg{} = Msg, #?Ctx{} = Ctx) ->
    gen_fsm:send_event(Session, {route, Msg, Ctx}).


%%% ==========================================================================
%%% Behaviour gen_fsm Callbacks
%%% ==========================================================================

%%% --------------------------------------------------------------------------
%%% Standard callbacks
%%% --------------------------------------------------------------------------

init([SessionToken]) ->
    lager:debug("Starting session ~p...", [SessionToken]),
    process_flag(trap_exit, true),
    bootstrap(wait_login, #?St{token = SessionToken}).

handle_event(Event, StateName, State) ->
    lager:error("Unexpected event in state ~p: ~p", [StateName, Event]),
    stop(StateName, {unexpected_event, StateName, Event}, State).


handle_sync_event(Event, From, StateName, State) ->
    lager:error("Unexpected event from ~p in state ~p: ~p",
                [From, StateName, Event]),
    stop_reply({error, unexpected_event}, StateName,
               {unexpected_event, StateName, From, Event}, State).


handle_info({'EXIT', _Conn, _Reason} = Event, StateName, State) ->
    ?MODULE:StateName(Event, State);

handle_info(Info, StateName, State) ->
    lager:warning("Unexpected message in state ~p: ~p", [StateName, Info]),
    continue(StateName, State).


terminate(Reason, StateName, _State) ->
    lager:debug("Terminating session in state ~p: ~p", [StateName, Reason]),
    ok.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%% --------------------------------------------------------------------------
%%% wait_login state callbacks
%%% --------------------------------------------------------------------------

wait_login({route, Msg, Ctx}, State) ->
    lager:debug("Routing through wait_login session: ~p", [Msg]),
    case route_login(Msg, Ctx, State) of
        {stop, NewState} -> stop(wait_login, normal, NewState);
        %{error, Reason, NewState} -> stop(wait_login, Reason, NewState);
        {ok, NewState} -> continue(wait_login, NewState)
    end;

wait_login(login_timeout, State) ->
    lager:debug("Session ~p login time exausted, terminating...", [State#?St.token]),
    stop(wait_login, normal, State);

wait_login(Event, State) ->
    handle_event(Event, wait_login, State).


wait_login({bind_context, Ctx}, _From, State) ->
    #?Ctx{conn = Conn, user = User, policy = Policy} = Ctx,
    lager:debug("Binding session to user ~p...", [User]),
    case erod_connection:bind_context(Ctx) of
        {error, _Reason} = Error-> reply(Error, wait_login, State);
        ok ->
            try erlang:link(Conn) of
                true ->
                    next_reply({ok, State#?St.token}, wait_login, established,
                               State#?St{conn = Conn, user = User, policy = Policy})
            catch
                error:badarg ->
                    % The connection is dead without sending any token,
                    % the reconnection is impossible. R.I.P
                    Error = {internal_error, connection_died},
                    stop_reply({error, Error}, wait_login, Error, State)
            end
    end;

wait_login({disband_context, #?Ctx{user = User}}, _From,
           #?St{conn = undefined, user = undefined} = State) ->
    lager:debug("Session binding with user ~p canceled", [User]),
    reply(ok, wait_login, State);

wait_login(Event, From, State) ->
    handle_sync_event(Event, From, wait_login, State).


%%% --------------------------------------------------------------------------
%%% established state callbacks
%%% --------------------------------------------------------------------------

established({route, Msg, #?Ctx{conn = Conn, sess = Sess, user = User} = Ctx},
            #?St{conn = Conn, user = User} = State) when Sess =:= self() ->
    lager:debug("Routing through established session: ~p", [Msg]),
    case route_established(Msg, Ctx, State) of
        %{stop, NewState} -> stop(established, normal, NewState);
        %{error, Reason, NewState} -> stop(estalished, Reason, NewState);
        {ok, NewState} -> continue(established, NewState)
    end;

established({route, Msg, Ctx}, State) ->
    erod_context:discard(Ctx, Msg),
    continue(established, State);

established({'EXIT', Conn, _}, #?St{conn = Conn} = State) ->
    lager:debug("Connection ~p died, waiting for reconnection...", [Conn]),
    next(established, wait_reconnect,
         State#?St{conn = undefined, policy = undefined});

established({'EXIT', User, _}, #?St{user = User} = State) ->
    lager:debug("User ~p died while in state established, committing suicide...", [User]),
    stop(established, user_died,
         State#?St{conn = undefined, user = undefined, policy = undefined});

established(Event, State) ->
    handle_event(Event, established, State).


established({bind_context, _Ctx}, _From, State) ->
    % Use and internal_error because this is NOT supposed to happen
    Error = {internal_error, session_already_connected},
    reply({error, Error}, established, State);

established({disband_context, #?Ctx{conn = Conn, user = User} = Ctx}, _From,
            #?St{conn = Conn, user = User} = State) ->
    lager:debug("Detaching session from user ~p...", [User]),
    erod_connection:disband_context(Ctx),
    catch erlang:unlink(Conn),
    stop_reply(ok, established, normal,
               State#?St{conn = undefined, user = undefined, policy = undefined});

established({disband_context, Ctx}, _From, State) ->
    lager:warning("Cannot detach from unknown context: ~p...", [Ctx]),
    reply({error, {internal_error, bad_context}}, established, State);

established(Event, From, State) ->
    handle_sync_event(Event, From, established, State).


%%% --------------------------------------------------------------------------
%%% wait_reconnect state callback
%%% --------------------------------------------------------------------------

wait_reconnect({route, Msg, Ctx}, State) ->
    lager:debug("Routing through wait_reconnect session: ~p", [Msg]),
    case route_reconnect(Msg, Ctx, State) of
        %{stop, NewState} -> stop(wait_reconnect, normal, NewState);
        %{error, Reason, NewState} -> stop(wait_reconnect, Reason, NewState);
        {ok, NewState} -> continue(wait_reconnect, NewState)
    end;

wait_reconnect({'EXIT', User, _}, #?St{user = User} = State) ->
    lager:debug("User ~p died while in state wait_reconnect, committing suicide...", [User]),
    stop(wait_reconnect, user_died,
         State#?St{user = undefined, policy = undefined});

wait_reconnect(reconnect_timeout, State) ->
    lager:debug("Session ~p reconnection time exausted, terminating...", [State#?St.token]),
    stop(wait_reconnect, normal, State);

wait_reconnect(Event, State) ->
    handle_event(Event, established, State).


wait_reconnect({bind_context, #?Ctx{user = User} = Ctx}, _From,
               #?St{user = User} = State) ->
    #?Ctx{conn = Conn, policy = Policy} = Ctx,
    lager:debug("Re-binding session to user ~p...", [User]),
    case erod_connection:bind_context(Ctx) of
        {error, _Reason} = Error-> reply(Error, wait_reconnect, State);
        ok ->
            try erlang:link(Conn) of
                true ->
                    next_reply({ok, State#?St.token}, wait_reconnect, established,
                               State#?St{conn = Conn, policy = Policy})
            catch
                error:badarg ->
                    Error = {internal_error, connection_died},
                    reply({error, Error}, wait_reconnect, State)
            end
    end;

wait_reconnect({bind_context, Ctx}, _From, State) ->
    lager:warning("Cannot attach to unknown context: ~p...", [Ctx]),
    reply({error, {internal_error, bad_context}}, wait_reconnect, State);

wait_reconnect({disband_context, #?Ctx{user = User}}, _From,
               #?St{user = User} = State) ->
    lager:debug("Detaching session from user ~p...", [User]),
    stop_reply(ok, wait_reconnect, normal, State#?St{user = undefined});

wait_reconnect({disband_context, Ctx}, _From, State) ->
    lager:warning("Cannot dettach from unknown context: ~p...", [Ctx]),
    reply({error, {internal_error, bad_context}}, wait_reconnect, State);

wait_reconnect(Event, From, State) ->
    handle_sync_event(Event, From, wait_reconnect, State).


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

%%% --------------------------------------------------------------------------
%%% State machine functions
%%% --------------------------------------------------------------------------

bootstrap(wait_login, State) ->
    TimeRef = gen_fsm:send_event_after(?WAIT_LOGIN_TIMEOUT, login_timeout),
    {ok, wait_login, State#?St{login_timeref = TimeRef}}.


continue(StateName, State) ->
    {next_state, StateName, State}.

reply(Reply, StateName, State) ->
    {reply, Reply, StateName, State}.


stop(StateName, Reason, State) ->
    {stop, Reason, leave(StateName, State)}.


stop_reply(Reply, StateName, Reason, State) ->
    {stop, Reason, Reply, leave(StateName, State)}.


next(From, To, State) ->
    {next_state, To, enter(To, transition(From, To, leave(From, State)))}.


next_reply(Reply, From, To, State) ->
    {reply, Reply, To, enter(To, transition(From, To, leave(From, State)))}.


leave(wait_login, #?St{login_timeref = TimeRef} = State) ->
    _ = gen_fsm:cancel_timer(TimeRef),
    State#?St{login_timeref = undefined};

leave(wait_reconnect, #?St{reconnect_timeref = TimeRef} = State) ->
    _ = gen_fsm:cancel_timer(TimeRef),
    State#?St{reconnect_timeref = undefined};

leave(_StateName, State) ->
    State.


transition(wait_login, established, State) -> State;

transition(established, wait_reconnect, State) -> State;

transition(wait_reconnect, established, State) -> State.


enter(wait_reconnect, State) ->
    TimeRef = gen_fsm:send_event_after(?WAIT_RECONNECT_TIMEOUT, reconnect_timeout),
    State#?St{reconnect_timeref = TimeRef};

enter(_StateName, State) ->
    State.


%%% --------------------------------------------------------------------------
%%% Routing functions
%%% --------------------------------------------------------------------------

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


route_reconnect(#?Msg{type = request, cls = reconnect} = Req, Ctx, State) ->
    #?St{token = Token, user = User} = State,
    case erod_context:safe_decode_data(Ctx, Req, ?MsgRecReq) of
        {error, _Reason} -> {ok, State};
        {ok, #?Msg{data = #?MsgRecReq{session = Token}} = NewReq} ->
            erod_user:route(User, NewReq, Ctx#?Ctx{sess = self()}),
            {ok, State};
        {ok, NewReq} ->
            Error = {reconnect_error, {internal_error, bad_session}},
            erod_context:reply_error(Ctx, NewReq, Error),
            {ok, State}
    end;

route_reconnect(Msg, Ctx, State) ->
    erod_context:discard(Ctx, Msg),
    {ok, State}.
