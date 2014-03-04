%%% ==========================================================================
%%% @doc Session.
%%% @end
%%% @private
%%% ==========================================================================
-module(erod_session).

-behaviour(gen_fsm).

%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_context.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Start/Stop functions
-export([start_link/5]).

%%% API functions
-export([close/2]).

%%% Internal protocol functions
-export([perform/4]).

%%% Behaviour gen_fsm standard callbacks
-export([init/1,
         handle_event/3,
         handle_sync_event/4,
         handle_info/3,
         terminate/3,
         code_change/4]).

%%% Behaviout gen_fsm states callbacks
-export([unbound/2, unbound/3,
         bound/2, bound/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).

%%% The time the session will wait for a client to bind.
-define(UNBOUND_TIMEOUT, 30*60*1000).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {sess_id :: pos_integer(),
              user_id :: pos_integer(),
              user :: pid(),
              policy :: erod:policy(),
              token :: binary(),
              proxy :: erod:proxy() | undefined,
              mod :: module(),
              sub :: term(),
              unbound_timer :: reference() | undefined}).


%%% ==========================================================================
%%% Behaviour erod_session Specification
%%% ==========================================================================

-callback init(Options)
    -> State
    when Options :: term(), State :: term().


%%% ==========================================================================
%%% Start/Stop Functions
%%% ==========================================================================

start_link(SessId, UserId, UserPid, Policy, Token) ->
    gen_fsm:start_link(?MODULE, [SessId, UserId, UserPid, Policy, Token], []).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

close(Session, Reason) ->
    gen_fsm:sync_send_event(Session, {close, Reason}).


%%% ==========================================================================
%%% Internal Protocol Functions
%%% ==========================================================================

perform(Session, Action, Args, Ctx) ->
    gen_fsm:send_all_state_event(Session, {perform, Action, Args, Ctx}).


%%% ==========================================================================
%%% Behaviour gen_fsm Callbacks
%%% ==========================================================================

%%% --------------------------------------------------------------------------
%%% Standard callbacks
%%% --------------------------------------------------------------------------

init([SessId, UserId, UserPid, Policy, Token]) ->
    lager:info("Session ~p with token ~p started for user ~p.",
               [SessId, Token, UserId]),
    process_flag(trap_exit, true),
    {ok, AppName} = application:get_application(),
    {ok, {Mod, Opts}} = application:get_env(AppName, session_mod),
    bootstrap(unbound, #?St{sess_id = SessId, token = Token,
                            user_id = UserId, user = UserPid,
                            policy = Policy, mod = Mod,
                            sub = Mod:init(Opts)}).


handle_event({perform, Action, Args, Ctx}, StateName, State) ->
    perform_action(StateName, Action, Args, Ctx, State);

handle_event(Event, StateName, State) ->
    lager:error("Unexpected event in state ~p: ~p", [StateName, Event]),
    stop(StateName, {unexpected_event, StateName, Event}, State).


handle_sync_event({close, Reason}, _From, StateName, State) ->
    #?St{sess_id = SessId, token = Token} = State,
    lager:debug("Session ~p with token ~p is being closed: ~p",
                [SessId, Token, Reason]),
    stop_reply(ok, StateName, Reason, State);

handle_sync_event(Event, {From, _Ref}, StateName, State) ->
    lager:error("Unexpected event from ~p in state ~p: ~p",
                [From, StateName, Event]),
    stop_reply({error, unexpected_event}, StateName,
               {unexpected_event, StateName, From, Event}, State).


handle_info({'EXIT', User, _}, StateName, #?St{user = User} = State) ->
    #?St{sess_id = SessId, user_id = UserId} = State,
    lager:debug("Session ~p's user ~p died while ~p, committing suicide.",
                [SessId, UserId, StateName]),
    stop(StateName, user_died, State#?St{proxy = undefined});

handle_info(Info, StateName, #?St{proxy = undefined} = State) ->
    lager:warning("Unexpected message in state ~p: ~p", [StateName, Info]),
    continue(StateName, State);

handle_info(Info, StateName, #?St{proxy = Proxy} = State) ->
    case erod_proxy:handle_message(Info, Proxy) of
        ignored ->
            lager:warning("Unexpected message in state ~p: ~p",
                          [StateName, Info]),
            continue(StateName, State);
        {ok, NewProxy} ->
            continue(StateName, State#?St{proxy = NewProxy});
        {dead, Reason, NewProxy} ->
            erod_proxy:info("Session proxy died, removing binding: ~p",
                            [Reason], NewProxy),
            next(StateName, unbound, State#?St{proxy = undefined})
    end.


terminate(Reason, StateName, State) ->
    #?St{sess_id = SessId, user_id = UserId, token = Token} = State,
    lager:info("Session ~p for user ~p with token ~p terminated in state ~p: ~p",
               [SessId, UserId, Token, StateName, Reason]),
    ok.


code_change(_OldVsn, StateName, State, _Extra) ->
    {ok, StateName, State}.


%%% --------------------------------------------------------------------------
%%% unbound state callbacks
%%% --------------------------------------------------------------------------

unbound(unbound_timeout, State) ->
    #?St{user_id = UserId, sess_id = SessId, token = Token} = State,
    lager:debug("Session ~p for user ~p with token ~p time exausted.",
                [SessId, UserId, Token]),
    stop(unbound, normal, State);

unbound(Event, State) ->
    handle_event(Event, unbound, State).


unbound(Event, From, State) ->
    handle_sync_event(Event, From, unbound, State).


%%% --------------------------------------------------------------------------
%%% bound state callbacks
%%% --------------------------------------------------------------------------

bound(Event, State) ->
    handle_event(Event, bound, State).


bound(Event, From, State) ->
    handle_sync_event(Event, From, bound, State).


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

%%% --------------------------------------------------------------------------
%%% State machine functions
%%% --------------------------------------------------------------------------

bootstrap(unbound, State) ->
    TimeRef = gen_fsm:send_event_after(?UNBOUND_TIMEOUT, unbound_timeout),
    {ok, unbound, State#?St{unbound_timer = TimeRef}}.


continue(StateName, State) ->
    {next_state, StateName, State}.

%% reply(Reply, StateName, State) ->
%%     {reply, Reply, StateName, State}.


stop(StateName, Reason, State) ->
    {stop, Reason, leave(StateName, State)}.


stop_reply(Reply, StateName, Reason, State) ->
    {stop, Reason, Reply, leave(StateName, State)}.


next(From, To, State) ->
    {next_state, To, enter(To, transition(From, To, leave(From, State)))}.


%% next_reply(Reply, From, To, State) ->
%%     {reply, Reply, To, enter(To, transition(From, To, leave(From, State)))}.


leave(unbound, #?St{unbound_timer = TimeRef} = State) ->
    _ = gen_fsm:cancel_timer(TimeRef),
    State#?St{unbound_timer = undefined};

leave(bound, State) ->
    State.


transition(unbound, bound, State) -> State;

transition(bound, unbound, State) -> State.


enter(unbound, State) ->
    TimeRef = gen_fsm:send_event_after(?UNBOUND_TIMEOUT, unbound_timeout),
    State#?St{unbound_timer = TimeRef};

enter(bound, State) ->
    State.


%%% --------------------------------------------------------------------------
%%% Actions
%%% --------------------------------------------------------------------------

perform_action(StateName, restore, _Args, Ctx, State) ->
    %TODO: Support identity check
    #?St{sess_id = SID, user_id = UID, user = User, policy = Pol} = State,
    NewCtx = erod_context:'_attach'(UID, User, SID, self(), Pol, Ctx),
    erod_context:info("Session restored.", [], Ctx),
    erod_context:done(NewCtx),
    continue(StateName, State);

perform_action(unbound, bind, [Proxy |_],
               #?Ctx{user_id = UID, sess_id = SID} = Ctx,
               #?St{user_id = UID, sess_id = SID} = State) ->
    case erod_proxy:accept(Ctx, Proxy) of
        {error, Reason, _NewProxy} ->
            erod_context:error("Session failed to accept to the proxy: ~p",
                               [Reason], Ctx),
            erod_context:failed(internal_error, Ctx),
            continue(unbound, State);
        {ok, NewProxy} ->
            erod_context:info("Session bound.", [], Ctx),
            erod_context:done(Ctx),
            next(unbound, bound, State#?St{proxy = NewProxy})
    end;

perform_action(unbound, bind, _Args, Ctx, #?St{sess_id = SID} = State) ->
    erod_context:error("Cannot bind session ~p if not logged in.", [SID], Ctx),
    erod_context:failed(not_authenticated, Ctx),
    continue(unbound, State);

perform_action(bound, bind, _Args,
               #?Ctx{user_id = UID, sess_id = SID} = Ctx,
               #?St{user_id = UID, sess_id = SID} = State) ->
    %TODO: Mayeb we want to close the old proxy and bind the new one...
    erod_context:error("Session already bound.", [], Ctx),
    erod_context:failed(already_bound, Ctx),
    continue(bound, State);

perform_action(bound, bind, _Args, Ctx, #?St{sess_id = SID} = State) ->
    erod_context:error("Cannot bind session ~p if not logged in.", [SID], Ctx),
    erod_context:failed(not_authenticated, Ctx),
    continue(bound, State);

perform_action(StateName, Action, Args, Ctx, State) ->
    erod_context:error("Session in state ~p doesn't know how to perform "
                       "action ~p with arguments ~p.",
                       [StateName, Action, Args], Ctx),
    erod_context:failed(unknown_action, Ctx),
    continue(StateName, State).
