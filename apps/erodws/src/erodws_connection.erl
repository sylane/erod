-module(erodws_connection).

-behaviour(cowboy_websocket_handler).

-include("erodws_protocol.hrl").

-export([notify/4,
         send_error/2,
         send_error_reply/4,
         send_result_reply/5,
         attach_context/2,
         release_context/1,
         close_connection/2]).

-export([init/3,
         websocket_init/3,
         websocket_handle/3,
         websocket_info/3,
         websocket_terminate/3]).

-define(St, ?MODULE).
-define(PING_PERIOD, 3000).
-define(INACTIVITY_TIMEOUT, 6000).
-define(CALL_TIMEOUT, 2000).

-record(?St, {proto :: erodws_protocol:protocol(),
              ping_timer :: reference() | undefined}).


notify(Connection, Cls, Fmt, Data) ->
    Connection ! {notify, Cls, Fmt, Data},
    ok.


send_error(Connection, Error) ->
    Connection ! {send_error, Error},
    ok.


send_error_reply(Connection, Cls, Id, Error) ->
    Connection ! {send_error_reply, Cls, Id, Error},
    ok.


send_result_reply(Connection, Cls, Id, Status, Data) ->
    Connection ! {send_result_reply, Cls, Id, Status, Data},
    ok.


attach_context(Connection, Ctx) ->
    Connection ! {attach_context, Ctx},
    ok.


release_context(Connection) ->
    Connection ! release_context,
    ok.


close_connection(Connection, Reason) ->
    Connection ! {close_connection, Reason},
    ok.


init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.


websocket_init(_Transport, Req, {ProtoMod, ProtoOpts}) ->
    {ok, NewReq, Proto} = erodws_protocol:new(Req, ProtoMod, ProtoOpts),
    State = schedule_keepalive(#?St{proto = Proto}),
    {ok, cowboy_req:compact(NewReq), State, ?INACTIVITY_TIMEOUT}.


websocket_handle({pong, <<>>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, <<"\n">>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, Json}, Req, #?St{proto = Proto} = State) ->
    case erodws_protocol:handle_packet(json, Json, Proto) of
        {ok, NewProto} -> {ok, Req, State#?St{proto = NewProto}};
        {error, _Reason, NewProto} ->
            {shutdown, Req, State#?St{proto = NewProto}}
    end;

websocket_handle(Frame, Req, State) ->
    lager:error("Unexpected websocket frame: ~p", [Frame]),
    {ok, Req, State}.


websocket_info(keepalive, Req, State) ->
    {reply, ping, Req, schedule_keepalive(State)};

websocket_info({notify, _Cls, _Fmt, _Data}, Req, State) ->
    %TODO
    {ok, Req, State};

websocket_info({send_error, Cls, Err}, Req, #?St{proto = Proto} = State) ->
    {ok, Data, NewProto} = erodws_protocol:encode_error(json, Cls, Err, Proto),
    {reply, {text, Data}, Req, State#?St{proto = NewProto}};

websocket_info({send_error_reply, C, I, Err}, Req, #?St{proto = P} = State) ->
    {ok, Data, P2} = erodws_protocol:encode_error_reply(json, C, I, Err, P),
    {reply, {text, Data}, Req, State#?St{proto = P2}};

websocket_info({send_result_reply, C, I, S, Res}, Req, State) ->
    #?St{proto = P} = State,
    {ok, Data, P2} = erodws_protocol:encode_result_reply(json, C, I, S, Res, P),
    {reply, {text, Data}, Req, State#?St{proto = P2}};

websocket_info({attach_context, Ctx}, Req, #?St{proto = Proto} = State) ->
    {ok, Req, State#?St{proto = erodws_protocol:attach_context(Ctx, Proto)}};

websocket_info(release_context, Req, #?St{proto = Proto} = State) ->
    {ok, Req, State#?St{proto = erodws_protocol:release_context(Proto)}};

websocket_info({close_connection, Reason}, Req, #?St{proto = Proto} = State) ->
    {shutdown, Req, State#?St{proto = erodws_protocol:shutdown(Reason, Proto)}};

websocket_info(Info, Req, State) ->
    lager:error("Unexpected message: ~p", [Info]),
    {ok, Req, State}.


websocket_terminate(Reason, _Req, #?St{proto = Proto} = State) ->
    _ = erodws_protocol:terminated(Reason, Proto),
    _ = cancel_keepalive(State),
    ok.


schedule_keepalive(State) ->
    State#?St{ping_timer = erlang:send_after(?PING_PERIOD, self(), keepalive)}.


cancel_keepalive(State) ->
    _ = erlang:cancel_timer(State#?St.ping_timer),
    State#?St{ping_timer = undefined}.
