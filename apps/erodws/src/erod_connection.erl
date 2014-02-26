-module(erod_connection).

-behaviour(cowboy_websocket_handler).

-include("erodws_internal.hrl").

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
              ctx :: erod_context() | undefined}).

%%% FIXME: A connection without session will never die.
%%% FIXME: A connection spawns a session every login message.
%%% TODO: Generic Abuse detection


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
    lager:info("Starting websocket handler...", []),
    {ok, cowboy_req:compact(Req),
     schedule_keepalive(#?St{}), ?INACTIVITY_TIMEOUT}.


websocket_handle({pong, <<>>}, Req, State) ->
    {ok, Req, State};

websocket_handle({text, <<"\n">>}, Req, State) ->
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
        {error, Reason, Result, NewReq, NewState} ->
            From ! {'$ack', Ref, Result},
            {stop, Reason, NewReq, NewState}
    end;

websocket_info(Info, Req, State) ->
    lager:error("Unexpected message: ~p", [Info]),
    {ok, Req, State}.


websocket_terminate(_Reason, _Req, State) ->
    lager:info("Terminating websocket handler...", []),
    _ = cancel_keepalive(State),
    ok.


websocket_call({bind_context, #?Ctx{sess = Sess} = Ctx}, Req,
               #?St{ctx = undefined} = State) ->
    lager:debug("Binding connection to session ~p...", [Sess]),
    {ok, ok, Req, State#?St{ctx = Ctx}};

websocket_call({bind_context, #?Ctx{sess = NewSess}}, Req,
               #?St{ctx = CurrSess} = State) ->
    lager:warning("Cannot bind connection to session ~p, "
                  "already bound to session ~p...", [NewSess, CurrSess]),
    {ok, {error, {internal_error, already_bound}}, Req, State};

websocket_call({disband_context, #?Ctx{sess = Sess} = Ctx}, Req,
               #?St{ctx = Ctx} = State) ->
    lager:debug("Detaching connection from session ~p...", [Sess]),
    catch erlang:unlink(Sess),
    {ok, ok, Req, State#?St{ctx = undefined}};

websocket_call({disband_context, Ctx}, Req, State) ->
    lager:warning("Cannot detach from unknown context: ~p...", [Ctx]),
    {ok, {error, {internal_error, bad_context}}, Req, State};

websocket_call(Request, Req, State) ->
    lager:error("Unexpected call: ~p", [Request]),
    {error, {unexpected_call, Request}, {error, unexpected_call}, Req, State}.


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
    _ = erlang:cancel_timer(State#?St.ping_timer),
    State#?St{ping_timer = undefined}.


decode_message(Json, State) ->
    try erod_message:decode(json, Json) of
        Msg -> handle_message(Msg, State)
    catch
        error:Error ->
            {reply, erod_message:encode_error(json, protocol, Error), State}
    end.


handle_message(Msg, #?St{ctx = undefined} = State) ->
    Ctx = #?Ctx{conn = self(), fmt = json},
    lager:debug("Routing through context-less connection: ~p", [Msg]),
    route_without_context(Msg, Ctx, State);

handle_message(Msg, #?St{ctx = Ctx} = State) ->
    lager:debug("Routing through connection: ~p", [Msg]),
    route_with_context(Msg, Ctx, State).


route_without_context(#?Msg{type = request, cls = login} = Req, Ctx, State) ->
    route_login(Req, Ctx, State);

route_without_context(#?Msg{type = request, cls = reconnect} = Req, Ctx, State) ->
    try ?MsgRecReq:decode(props, Req#?Msg.data) of
        Data -> route_reconnect(Req#?Msg{data = Data}, Ctx, State)
    catch
        error:Error ->
            {reply, erod_message:encode_error_reply(json, Req, Error), State}
    end;

route_without_context(#?Msg{type = request} = Req, _Ctx, State) ->
    lager:warning("Packet received without session: ~p", [Req]),
    {reply, erod_message:encode_error_reply(json, Req, no_session), State};

route_without_context(Msg, _Ctx, State) ->
    lager:warning("Packet received without session: ~p", [Msg]),
    {ok, State}.


route_login(Req, Ctx, State) ->
    case erod_session_manager:new_session() of
        {error, Reason} ->
            Error = {login_error, {internal_error, Reason}},
            {reply, erod_message:encode_error_reply(json, Req, Error), State};
        {ok, Session} ->
            erod_session:route(Session, Req, Ctx),
            {ok, State}
    end.


route_reconnect(Req, Ctx, State) ->
    #?Msg{data = #?MsgRecReq{session = Token}} = Req,
    case erod_session_manager:find_session(Token) of
        {error, Reason} ->
            Error = {reconnect_error, Reason},
            {reply, erod_message:encode_error_reply(json, Req, Error), State};
        {ok, Session} ->
            erod_session:route(Session, Req, Ctx),
            {ok, State}
    end.


route_with_context(#?Msg{type = request, cls = get_content} = Req, Ctx, State) ->
    try ?MsgGetConReq:decode(props, Req#?Msg.data) of
        Data -> get_content(Req#?Msg{data = Data}, Ctx, State)
    catch
        error:Error ->
            {reply, erod_message:encode_error_reply(json, Req, Error), State}
    end;

route_with_context(#?Msg{type = request, cls = get_children} = Req, Ctx, State) ->
    try ?MsgGetChiReq:decode(props, Req#?Msg.data) of
        Data -> get_children(Req#?Msg{data = Data}, Ctx, State)
    catch
        error:Error ->
            {reply, erod_message:encode_error_reply(json, Req, Error), State}
    end;

route_with_context(#?Msg{type = request, cls = logout} = Req, Ctx, State) ->
    erod_session:route(Ctx#?Ctx.sess, Req, Ctx),
    {ok, State};

route_with_context(#?Msg{type = request} = Req, _Ctx, State) ->
    lager:warning("Unexpected packet received: ~p", [Req]),
    {reply, erod_message:encode_error_reply(json, Req, unexpected), State};

route_with_context(Msg, _Ctx, State) ->
    lager:warning("Unexpected packet received: ~p", [Msg]),
    {ok, State}.


get_content(Req, Ctx, State) ->
    #?MsgGetConReq{key = Key, ver = Ver, subscribe = Subs} =  Req#?Msg.data,
    Watcher = if Subs -> Ctx#?Ctx.sess;
                 true -> undefined
              end,
    case erod_registry:get_content(Key, Ver, Watcher) of
        {error, Reason} ->
            {reply, erod_message:encode_error_reply(json, Req, Reason), State};
        unchanged ->
            #?Msg{id = Id, cls = Cls} = Req,
            Result = #?Msg{type = result, id = Id, cls = Cls},
            Packet = erod_message:encode(json, Result),
            {reply, Packet, State};
        #erod_content{} = Content ->
            #?Msg{type = request, id = Id, cls = Cls} = Req,
            Data = #?MsgGetConRes{content = Content},
            Result = #?Msg{type = result, id = Id, cls = Cls, data = Data},
            Packet = erod_message:encode(json, Result),
            {reply, Packet, State}
    end.


get_children(Req, Ctx, State) ->
    #?MsgGetChiReq{key = Key, ver = Ver, view = ViewId,
                   page = PageId, subscribe = Subs} =  Req#?Msg.data,
    Watcher = if Subs -> Ctx#?Ctx.sess;
                 true -> undefined
              end,
    case erod_registry:get_children(Key, ViewId, PageId, Ver, Watcher) of
        {error, Reason} ->
            {reply, erod_message:encode_error_reply(json, Req, Reason), State};
        unchanged ->
            #?Msg{id = Id, cls = Cls} = Req,
            Result = #?Msg{type = result, id = Id, cls = Cls},
            Packet = erod_message:encode(json, Result),
            {reply, Packet, State};
        #erod_page{} = Page ->
            #?Msg{type = request, id = Id, cls = Cls} = Req,
            Data = #?MsgGetChiRes{page = Page},
            Result = #?Msg{type = result, id = Id, cls = Cls, data = Data},
            Packet = erod_message:encode(json, Result),
            {reply, Packet, State}
    end.
