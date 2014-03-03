-module(erodws_context).

-include("erodws_protocol.hrl").

-behaviour(erod_context).

-export([new/3,
         clone/4]).

-export([init/1,
         format_log_id/2,
         done/4,
         failed/4,
         attached/2,
         released/1]).


-define(St, ?MODULE).
-record(?St, {conn, peer, replied = false, type, cls, id}).


new(Connection, Peer, Msg) ->
    erod_context:new(?MODULE, {Connection, Peer, Msg}).


clone(Connection, Peer, Msg, Ctx) ->
    erod_context:clone(?MODULE, {Connection, Peer, Msg}, Ctx).


init({Conn, Peer, #?Msg{type = T, cls = C, id = I}}) ->
    #?St{conn = Conn, peer = Peer, type = T, cls = C, id = I}.


format_log_id(LogId, #?St{peer = undefined}) ->
    <<LogId/binary, ",?">>;

format_log_id(LogId, #?St{peer = Peer}) ->
    PeerBin = erodlib:peer2bin(Peer),
    <<LogId/binary, ",", PeerBin/binary>>.


done(login, Result, Ctx, #?St{cls = login} = St) ->
    send_result_reply(login, Result, Ctx, St);

done(bind, _, _, #?St{cls = login} = St) ->
    St;

done(restore, _, Ctx, #?St{cls = reconnect} = St) ->
    send_result_reply(restore, undefined, Ctx, St);

done(bind, _, _, #?St{cls = reconnect} = St) ->
    St;

done(logout, _, Ctx, #?St{cls = logout} = St) ->
    send_result_reply(logout, undefined, Ctx, St);

done(get_content, Result, Ctx, #?St{cls = get_content} = St) ->
    send_result_reply(get_content, Result, Ctx, St);

done(get_children, Result, Ctx, #?St{cls = get_children} = St) ->
    send_result_reply(get_children, Result, Ctx, St);

done(Action, Result, Ctx, #?St{type = Type, cls = Cls} = St) ->
    erod_context:error("Websocket unexpected protocol action ~p "
                       "succeed for ~p ~p with result: ~p",
                       [Action, Cls, Type, Result], Ctx),
    send_error_reply(Action, internal_error, Ctx, St).


failed(login, Reason, Ctx, #?St{cls = login} = St) ->
    send_error_reply(login, Reason, Ctx, St);

failed(bind, Reason, _, #?St{cls = login} = St) ->
    close_connection({internal_error, {bind_failed, Reason}}, St);

failed(restore, Error, Ctx, #?St{cls = reconnect} = St) ->
    send_error_reply(restore, Error, Ctx, St);

failed(bind, Reason, _, #?St{cls = reconnect} = St) ->
    close_connection({internal_error, {bind_failed, Reason}}, St);

failed(logout, Reason, Ctx, #?St{cls = logout} = St) ->
    send_error_reply(logout, Reason, Ctx, St);

failed(get_content, Reason, Ctx, #?St{cls = get_content} = St) ->
    send_error_reply(get_content, Reason, Ctx, St);

failed(get_children, Reason, Ctx, #?St{cls = get_children} = St) ->
    send_error_reply(get_children, Reason, Ctx, St);

failed(undefined, Reason, Ctx, St) ->
    % Errors happening before any action got performed
    send_error_reply(undefined, Reason, Ctx, St);

failed(Action, Reason, Ctx, #?St{type = Type, cls = Cls} = St) ->
    erod_context:error("Websocket unexpected protocol action ~p "
                       "failed for ~p ~p with reason: ~p",
                       [Action, Cls, Type, Reason], Ctx),
    send_error_reply(Action, internal_error, Ctx, St).


attached(Ctx, #?St{conn = Conn} = St) ->
    erodws_connection:attach_context(Conn, Ctx),
    St.


released(#?St{conn = Conn} = St) ->
    erodws_connection:release_context(Conn),
    St.



send_result_reply(Act, _Res,  Ctx, #?St{replied = true} = St) ->
    lager:warning("Reply for action ~p already sent to ~s.",
                  [Act, erod_context:log_id(Ctx)]),
    St;

send_result_reply(_, Res, _, #?St{conn = Conn, cls = Cls, id = Id} = St) ->
    erodws_connection:send_result_reply(Conn, Cls ,Id, Res),
    St#?St{replied = true}.


send_error_reply(Act, Err,  Ctx, #?St{replied = true} = St) ->
    lager:warning("Error reply for action ~p already sent to ~s: ~p",
                  [Act, erod_context:log_id(Ctx), Err]),
    St;

send_error_reply(_, Err,  _, #?St{conn = Conn, cls = Cls, id = Id} = St) ->
    erodws_connection:send_error_reply(Conn, Cls ,Id, Err),
    St#?St{replied = true}.


close_connection(Reason, #?St{conn = Conn} = St) ->
    erodws_connection:close_connection(Conn, Reason),
    St.
