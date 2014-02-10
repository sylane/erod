-module(erod_context).

-include("erod_internal.hrl").

-export([link_session/2]).
-export([unlink_session/2]).
-export([send/2]).
-export([safe_send/2]).
-export([send_error/3]).
-export([reply/3]).
-export([safe_reply/3]).
-export([reply_error/3]).
-export([discard/2]).
-export([safe_decode/4]).
-export([safe_decode_data/3]).


link_session(#?Ctx{connection = Conn}, Session) ->
    try erlang:link(Conn) of
        true -> erod_connection:link_session(Conn, Session)
    catch
        error:noproc -> {error, noproc}
    end.


unlink_session(#?Ctx{connection = Conn}, Session) ->
    try erlang:unlink(Conn) of
        true ->
            erod_connection:unlink_session(Conn, Session),
            receive {'EXIT', Conn, _} -> ok after 0 -> ok end
    catch
        error:noproc -> {error, noproc}
    end.


send(#?Ctx{connection = Conn, format = Fmt}, Msg) ->
    Packet = Msg:encode(Fmt, Msg),
    erod_connection:send(Conn, Packet).

safe_send(#?Ctx{connection = Conn, format = Fmt}, Msg) ->
    try Msg:encode(Fmt, Msg) of
        Packet -> erod_connection:send(Conn, Packet)
    catch
        error:{format_error, _} = Error -> {error, Error}
    end.


send_error(#?Ctx{connection = Conn, format = Fmt}, Class, Error) ->
    Packet = erod_message:encode_error(Fmt, Class, Error),
    erod_connection:send(Conn, Packet).


reply(#?Ctx{connection = Conn, format = Fmt},
      #?Msg{type = request, id = Id, cls = Cls}, Data) ->
    Result = #?Msg{type = result, id = Id, cls = Cls, data = Data},
    Packet = erod_message:encode(Fmt, Result),
    erod_connection:send(Conn, Packet).


safe_reply(#?Ctx{connection = Conn, format = Fmt},
           #?Msg{type = request, id = Id, cls = Cls}, Data) ->
    Result = #?Msg{type = result, id = Id, cls = Cls, data = Data},
    try erod_message:encode(Fmt, Result) of
        Packet -> erod_connection:send(Conn, Packet)
    catch
        error:{format_error, _} = Error -> {error, Error}
    end.


reply_error(#?Ctx{connection = Conn, format = Fmt},
            #?Msg{type = request, id = Id, cls = Cls}, Error) ->
    Packet = erod_message:encode_error(Fmt, Cls, Id, Error),
    erod_connection:send(Conn, Packet).


discard(Ctx, #?Msg{type = request} = Req) ->
    lager:warning("Unexpected request discarded: ~p", [Req]),
    reply_error(Ctx, Req, unexpected_request);

discard(_Ctx, #?Msg{} = Msg) ->
    lager:warning("Unexpected message discarded: ~p", [Msg]),
    ok.


safe_decode(#?Ctx{}, #?Msg{}, Props, Module)
  when is_tuple(Props), element(1, Props) =:= Module ->
    {ok, Props};

safe_decode(#?Ctx{connection = Conn, format = Fmt},
       #?Msg{type = request, id = Id, cls = Cls},
       Props, Module) ->
    try {ok, Module:decode(props, Props)}
    catch
        error:{format_error, _} = Error ->
            Packet = erod_message:encode_error(Fmt, Cls, Id, Error),
            erod_connection:send(Conn, Packet),
            {error, Error}
    end;

safe_decode(#?Ctx{}, _Msg, Props, Module) ->
    try {ok, Module:decode(props, Props)}
    catch
        error:{format_error, _} = Error ->
            {error, Error}
    end.


safe_decode_data(Ctx, Msg, Module) ->
    case safe_decode(Ctx, Msg, Msg#?Msg.data, Module) of
        {ok, Data} -> {ok, Msg#?Msg{data = Data}};
        {error, _} = Error -> Error
    end.
