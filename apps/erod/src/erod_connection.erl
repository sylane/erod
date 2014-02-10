-module(erod_connection).

-behaviour(cowboy_websocket_handler).

-include("erod_internal.hrl").

-export([link_session/2]).
-export([unlink_session/2]).
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
              session :: pid() | undefined}).


link_session(Connection, Session) ->
    call(Connection, {link_session, Session}).


unlink_session(Connection, Session) ->
    call(Connection, {unlink_session, Session}).


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


websocket_call({link_session, Session}, Req, State) ->
    lager:debug("Connection linked to session ~p", [Session]),
    {ok, ok, Req, State#?St{session = Session}};

websocket_call({unlink_session, Session}, Req,
               #?St{session = Session} = State) ->
    lager:debug("Connection link to session ~p removed", [Session]),
    {ok, ok, Req, State#?St{session = undefined}};

websocket_call({unlink_session, Session}, Req, State) ->
    lager:warning("Connection asked to unlink from unknown session ~p", [Session]),
    {ok, ok, Req, State};

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


handle_message(Msg, State) ->
    Ctx = #?Ctx{connection = self(), format = json},
    lager:debug("Routing message: ~p", [Msg]),
    route_message(Msg, Ctx, State).


route_message(#?Msg{cls = Class} = Msg, Ctx,
              #?St{session = undefined} = State)
  when Class =:= login; Class =:= reconnect ->
    erod_session_manager:route(Msg, Ctx),
    {ok, State};

route_message(#?Msg{type = request} = Req, _Ctx,
              #?St{session = undefined} = State) ->
    lager:warning("Packet received without session: ~p", [Req]),
    {reply, erod_message:encode_error_reply(json, Req, no_session), State};

route_message(Msg, _Ctx, #?St{session = undefined} = State) ->
    lager:warning("Packet received without session: ~p", [Msg]),
    {ok, State};

route_message(Msg, Ctx, State) ->
    erod_session:route(State#?St.session, Msg, Ctx),
    {ok, State}.
