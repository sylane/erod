-module(erod_connection).

-behaviour(cowboy_websocket_handler).

-include("erod_internal.hrl").

-export([bind_context/1]).
-export([disband_context/1]).
-export([send/2]).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-define(St, ?MODULE).
-define(PING_PERIOD, 3000).
-define(INACTIVITY_TIMEOUT, 6000).
-define(CALL_TIMEOUT, 2000).


-record(?St, {ping_timer :: reference() | undefined,
              context :: erod_context() | undefined}).


bind_context(#?Ctx{conn = Connection} = Ctx) ->
    call(Connection, {bind_context, Ctx}).

disband_context(#?Ctx{conn = Connection} = Ctx) ->
    call(Connection, {disband_context, Ctx}).


send(Connection, Data) ->
    Connection ! {send, Data},
    ok.


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_Transport, Req, []) ->
    lager:debug("Starting websocket handler...", []),
    {ok, cowboy_req:compact(Req),
     schedule_keepalive(#?St{}), ?INACTIVITY_TIMEOUT}.


websocket_handle({pong, <<>>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, Json}, Req, State) ->
    case decode_message(Json, State) of
        {ok, NewState} -> {ok, Req, NewState};
        {reply, Msg, NewState} -> {reply, {text, Msg}, Req, NewState}
    end;

websocket_handle(Frame, Req, State) ->
    lager:error("Unexpected frame: ~p", [Frame]),
    {ok, Req, State}.


websocket_info(keepalive, Req, State) ->
    {reply, ping, Req, schedule_keepalive(State)};

websocket_info({send, Data}, Req, State) ->
    {reply, {text, Data}, Req, State};

websocket_info({'$call', From, Ref, Request}, Req, State) ->
    case websocket_call(Request, Req, State) of
        {ok, Result, NewReq, NewState} ->
            From ! {'$ack', Ref, Result},
            {ok, NewReq, NewState};
        {reply, Reply, Result, NewReq, NewState} ->
            From ! {'$ack', Ref, Result},
            {reply, Reply, NewReq, NewState}
    end;

websocket_info(Info, Req, State) ->
    lager:error("Unexpected message: ~p", [Info]),
    {ok, Req, State}.


websocket_terminate(_Reason, _Req, State) ->
    lager:debug("Terminating websocket handler...", []),
    cancel_keepalive(State),
    ok.


websocket_call({bind_context, #?Ctx{sess = Sess} = Ctx}, Req,
               #?St{context = undefined} = State) ->
    lager:debug("Binding connection to session ~p...", [Sess]),
    {ok, ok, Req, State#?St{context = Ctx}};

websocket_call({disband_context, #?Ctx{sess = Sess} = Ctx}, Req,
               #?St{context = Ctx} = State) ->
    lager:debug("Detaching connection from session ~p...", [Sess]),
    {ok, ok, Req, State#?St{context = undefined}};

websocket_call(Request, Req, State) ->
    lager:error("Unexpected call: ~p", [Request]),
    {stop, {unexpected_call, Request}, {error, unexpected_call}, Req, State}.


call(Connection, Msg) ->
    Ref = make_ref(),
    Connection ! {'$call', self(), Ref, Msg},
    receive
        {'$ack', Ref, Result} -> Result
    after ?CALL_TIMEOUT -> timeout
    end.


schedule_keepalive(State) ->
    State#?St{ping_timer = erlang:send_after(?PING_PERIOD, self(), keepalive)}.


cancel_keepalive(State) ->
    erlang:cancel_timer(State#?St.ping_timer),
    State#?St{ping_timer = undefined}.


decode_message(Json, State) ->
    try erod_message:decode(json, Json) of
        Msg -> handle_message(Msg, State)
    catch
        error:Error ->
            {reply, erod_message:encode_error(json, protocol, Error), State}
    end.


handle_message(Msg, #?St{context = undefined} = State) ->
    Ctx = #?Ctx{conn = self(), fmt = json},
    lager:debug("Routing through context-less connection: ~p", [Msg]),
    route_without_context(Msg, Ctx, State);

handle_message(Msg, #?St{context = Ctx} = State) ->
    lager:debug("Routing through connection: ~p", [Msg]),
    route_message(Msg, Ctx, State).


route_without_context(#?Msg{type = request, cls = login} = Req, Ctx, State) ->
    case erod_session_manager:new_session() of
        {error, Reason} ->
            Error = {internal_error, Reason},
            {reply, erod_message:encode_error_reply(json, Req, Error), State};
        {ok, Session} ->
            erod_session:route(Session, Req, Ctx),
            {ok, State}
    end;

route_without_context(#?Msg{type = request} = Req, _Ctx, State) ->
    lager:warning("Packet received without session: ~p", [Req]),
    {reply, erod_message:encode_error_reply(json, Req, no_session), State};

route_without_context(Msg, _Ctx, State) ->
    lager:warning("Packet received without session: ~p", [Msg]),
    {ok, State}.


route_message(Msg, Ctx, State) ->
    erod_session:route(Ctx#?Ctx.sess, Msg, Ctx),
    {ok, State}.
