-module(erod_user).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/1]).

-export([route/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(St, ?MODULE).

-record(?St, {identity :: erod_user_identity(),
              user_id :: pos_integer(),
              sessions = [] :: [pid()] | []}).


start_link(UserIdent) ->
    gen_server:start_link(?MODULE, [UserIdent], []).


route(User, #?Msg{} = Msg, #?Ctx{} = Ctx) ->
    gen_server:cast(User, {route, Msg, Ctx}).


init([UserIdent]) ->
    lager:debug("Starting user ~p...", [UserIdent]),
    process_flag(trap_exit, true),
    case load_user(UserIdent) of
        {ok, State} -> {ok, State};
        {error, Reason} -> {stop, Reason}
    end.


handle_call(Request, From, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast({route, Msg, Ctx}, #?St{identity = Ident} = State) ->
    lager:debug("Routing through user ~p: ~p", [Ident, Msg]),
    {noreply, route_message(Msg, Ctx, State)};

handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info({'EXIT', Pid, _}, State) ->
    case has_session(Pid, State) of
        false -> {noreply, State};
        true ->
            lager:debug("Session ~p died...", [Pid]),
            %TODO: Do some session managment
            {noreply, del_session(Pid, State)}
    end;

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:debug("Terminating user: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



load_user(#?UserIdent{username = <<"foo">>}) ->
    {error, unknown_user};

load_user(UserIdent) ->
    % Load a user from the storage...
    _ = random:seed(erlang:now()),
    {ok, #?St{identity = UserIdent,
              user_id = random:uniform(999999999)}}.


make_login_result(SessionToken, State) ->
    UserId = State#?St.user_id,
    #?MsgLogRes{session = SessionToken,
                self = {user, UserId},
                rooms = {rooms, UserId},
                fav = {fav, UserId},
                root = {index, 0},
                home = undefined,
                config = [{media_server, <<"http://foo.bar.com/statics">>}]}.


authenticate(#?UserCred{identity = ReqIdent}, #?St{identity = ProcIdent})
  when ReqIdent =/= ProcIdent ->
    lager:error("User process for ~p received an authentication request "
                "for a different identity: ~p", [ProcIdent, ReqIdent]),
    {error, internal_error};

authenticate(#?UserCred{password = <<"foo">>}, _State) ->
    {error, invalid_credencial};

authenticate(_UserCred, _State) ->
    % Any other passwords will do
    ok.


has_session(Session, #?St{sessions = Sessions}) ->
    lists:member(Session, Sessions).


add_session(Session, #?St{sessions = Sessions} = State) ->
    State#?St{sessions = [Session |Sessions]}.


del_session(Session, #?St{sessions = Sessions} = State) ->
    State#?St{sessions = lists:delete(Session, Sessions)}.


route_message(#?Msg{type = request, cls = login} = Req, Ctx, State) ->
    case erod_context:safe_decode_data(Ctx, Req, ?MsgLogReq) of
        {error, _Reason} -> login_failed(Req, Ctx, undefined, State);
        {ok, NewReq} -> validate_login(NewReq, Ctx, State)
    end;

route_message(#?Msg{type = request, cls = reconnect} = Req, Ctx, State) ->
    case erod_context:safe_decode_data(Ctx, Req, ?MsgRecReq) of
        {error, _Reason} -> State;
        {ok, NewReq} -> validate_reconnect(NewReq, Ctx, State)
    end;

route_message(#?Msg{type = request, cls = logout} = Req, Ctx, State) ->
    perform_logout(Req, Ctx, State);

route_message(Msg, Ctx, State) ->
    erod_context:discard(Ctx, Msg),
    State.


validate_login(Req, #?Ctx{sess = undefined} = Ctx, State) ->
    lager:error("Received login request without any context's session"),
    login_failed(Req, Ctx, {internal_error, context_without_session}, State);

validate_login(#?Msg{data = LogReq} = Req,
               #?Ctx{sess = Sess} = Ctx, State) ->
    case has_session(Sess, State) of
        true ->
            lager:error("Session ~p already logged in with user ~p",
                        [Sess, State#?St.identity]),
            login_failed(Req, Ctx, {internal_error, session_already_exists}, State);
        false ->
            case authenticate(?MsgLogReq:as_credential(LogReq), State) of
                ok -> perform_login(Req, Ctx, State);
                {error, Reason} ->
                    login_failed(Req, Ctx, Reason, State)
            end
    end.


login_failed(_Req, #?Ctx{sess = Sess, conn = Conn} = Ctx, undefined, State) ->
    lager:info("Failed login for user ~p with session ~p through "
               "connection ~p", [State#?St.identity, Sess, Conn]),
    erod_session:disband_context(Ctx),
    State;

login_failed(Req, #?Ctx{sess = Sess, conn = Conn} = Ctx, Reason, State) ->
    lager:info("Failed login for user ~p with session ~p through "
               "connection ~p: ~p", [State#?St.identity, Sess, Conn, Reason]),
    erod_context:reply_error(Ctx, Req, {login_error, Reason}),
    erod_session:disband_context(Ctx),
    State.


perform_login(Req, #?Ctx{sess = Sess, conn = Conn} = Ctx, State) ->
    SessPol = #?SessPol{user_id = State#?St.user_id},
    NewCtx = Ctx#?Ctx{user = self(), policy = SessPol},
    case erod_session:bind_context(NewCtx) of
        {error, Reason} ->
            lager:info("Failed logging in user ~p with session ~p through "
                       "connection ~p: ~p",
                       [State#?St.identity, Sess, Conn, Reason]),
            erod_context:reply_error(Ctx, Req, {login_error, Reason}),
            State;
        {ok, SessionToken} ->
            try erlang:link(Sess) of
                true ->
                    lager:info("Successful login for user ~p with session ~p "
                               "through connection ~p",
                               [State#?St.identity, Sess, Conn]),
                    LogRes = make_login_result(SessionToken, State),
                    erod_context:reply(Ctx, Req, LogRes),
                    add_session(Sess, State)
            catch
                error:badarg ->
                    login_failed(Req, NewCtx, internal_error, State)
            end
    end.


validate_reconnect(Req, #?Ctx{sess = Sess, conn = Conn} = Ctx, State) ->
    case has_session(Sess, State) of
        true -> perform_reconnect(Req, Ctx, State);
        false ->
            lager:warning("Cannot rebind connection ~p "
                          "through unknown session ~p", [Conn, Sess]),
            Error = {reconnect_error, {internal_error, unknown_session}},
            erod_context:reply_error(Ctx, Req, Error),
            State
    end.



perform_reconnect(Req, #?Ctx{sess = Sess, conn = Conn} = Ctx, State) ->
    #?Msg{data = #?MsgRecReq{session = Token}} = Req,
    SessPol = #?SessPol{user_id = State#?St.user_id},
    NewCtx = Ctx#?Ctx{user = self(), policy = SessPol},
    case erod_session:bind_context(NewCtx) of
        {error, Reason} ->
            lager:info("Failed reconnecting user ~p with session ~p through "
                       "connection ~p: ~p",
                       [State#?St.identity, Sess, Conn, Reason]),
            erod_context:reply_error(Ctx, Req, {reconnect_error, Reason}),
            State;
        {ok, Token} ->
            lager:info("Successfuly reconnected user ~p with session ~p "
                       "through connection ~p",
                       [State#?St.identity, Sess, Conn]),
            erod_context:reply(Ctx, Req, undefined),
            State;
        {ok, OtherToken} ->
            %FIXME: Remove this defensive code.
            lager:error("Reconnection of user ~p with session ~p "
                        "through connection ~p went bad, session gave "
                        "token ~p when ~p was expected, terminating...",
                        [State#?St.identity, Sess, Conn, OtherToken, Token]),
            Error = {reconnect_error, {internal_error, bad_session}},
            erod_context:reply_error(Ctx, Req, Error),
            erod_session:disband_context(NewCtx),
            State
    end.


perform_logout(Req, #?Ctx{sess = Sess, user = User} = Ctx, State)
  when User =:= self(), Sess =/= undefined ->
    case has_session(Sess, State) of
        false ->
            lager:error("Received logout request for unknown session ~p", [Sess]),
            Error = {logout_error, {internal_error, unknown_session}},
            erod_context:reply_error(Ctx, Req, Error),
            State;
        true ->
            erod_session:disband_context(Ctx),
            erod_context:reply(Ctx, Req, undefined),
            del_session(Sess, State)
    end;

perform_logout(Req, Ctx, State) ->
    lager:error("Received logout request with invalid context: ~p", [Ctx]),
    Error = {logout_error, {internal_error, invalid_context}},
    erod_context:reply_error(Ctx, Req, Error),
    State.
