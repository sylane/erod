%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erodws.
%%%
%%% Erodws is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% ==========================================================================
%%% @copyright 2014 Sebastien Merle <s.merle@gmail.com>
%%% @author Sebastien Merle <s.merle@gmail.com>
%%% @doc TODO: Document module erodws_context.
%%% @end
%%% @private
%%% ==========================================================================

-module(erodws_context).

-author('Sebastien Merle').

-behaviour(erod_context).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include_lib("erod/include/erod_document.hrl").

-include("erodws_protocol.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/3,
         clone/4]).

%%% Behaviour erod_context callbacks
-export([init/1,
         format_log_id/2,
         done/4,
         failed/4,
         attached/2,
         released/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {conn, peer, replied = false, type, cls, id}).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new context for a message.
%% @end
%% -----------------------------------------------------------------
-spec new(Connection, Peer, Msg) -> Context
    when Connection :: pid(), Peer :: {inet:ip_address(), inet:port_number()},
         Msg :: erodws_message(), Context :: erod:context().
%% -----------------------------------------------------------------

new(Connection, Peer, Msg) ->
    erod_context:new(?MODULE, {Connection, Peer, Msg}).


%% -----------------------------------------------------------------
%% @doc Clones an existing message for a message.
%% @end
%% -----------------------------------------------------------------
-spec clone(Connection, Peer, Msg, Context) -> Context
    when Connection :: pid(), Peer :: {inet:ip_address(), inet:port_number()},
         Msg :: erodws_message(), Context :: erod:context().
%% -----------------------------------------------------------------

clone(Connection, Peer, Msg, Ctx) ->
    erod_context:clone(?MODULE, {Connection, Peer, Msg}, Ctx).


%%% ==========================================================================
%%% Behaviour erod_context Callacks
%%% ==========================================================================

init({Conn, Peer, #?Msg{type = T, cls = C, id = I}}) ->
    #?St{conn = Conn, peer = Peer, type = T, cls = C, id = I}.


format_log_id(LogId, #?St{peer = undefined}) ->
    <<LogId/binary, ",?">>;

format_log_id(LogId, #?St{peer = Peer}) ->
    PeerBin = erodlib:peer2bin(Peer),
    <<LogId/binary, ",", PeerBin/binary>>.


done(get_content, Result, Ctx, #?St{cls = get_content} = St) ->
    done_get_content(Result, Ctx, St);

done(get_children, Result, Ctx, #?St{cls = get_children} = St) ->
    done_get_children(Result, Ctx, St);

done(patch_content, Result, Ctx, #?St{cls = patch_content} = St) ->
    send_result_reply(patch_content, 200, Result, Ctx, St);

done(login, Result, Ctx, #?St{cls = login} = St) ->
    send_result_reply(login, 200, Result, Ctx, St);

done(bind, _, _, #?St{cls = login} = St) ->
    St;

done(restore, _, Ctx, #?St{cls = reconnect} = St) ->
    send_result_reply(restore, 200, undefined, Ctx, St);

done(bind, _, _, #?St{cls = reconnect} = St) ->
    St;

done(logout, _, Ctx, #?St{cls = logout} = St) ->
    send_result_reply(logout, 200, undefined, Ctx, St);

done(Action, Result, Ctx, #?St{type = Type, cls = Cls} = St) ->
    erod_context:error("Websocket unexpected protocol action ~p "
                       "succeed for ~p ~p with result: ~p",
                       [Action, Cls, Type, Result], Ctx),
    send_error_reply(Action, internal_error, Ctx, St).


failed(get_content, Reason, Ctx, #?St{cls = get_content} = St) ->
    send_error_reply(get_content, Reason, Ctx, St);

failed(get_children, Reason, Ctx, #?St{cls = get_children} = St) ->
    send_error_reply(get_children, Reason, Ctx, St);

failed(patch_content, Reason, Ctx, #?St{cls = patch_content} = St) ->
    send_error_reply(patch_content, Reason, Ctx, St);

failed(login, Reason, Ctx, #?St{cls = login} = St) ->
    send_error_reply(login, Reason, Ctx, St);

failed(bind, Reason, _, #?St{cls = login} = St) ->
    close_connection({bind_failed, Reason}, St);

failed(restore, Error, Ctx, #?St{cls = reconnect} = St) ->
    send_error_reply(restore, Error, Ctx, St);

failed(bind, Reason, _, #?St{cls = reconnect} = St) ->
    close_connection({bind_failed, Reason}, St);

failed(logout, Reason, Ctx, #?St{cls = logout} = St) ->
    send_error_reply(logout, Reason, Ctx, St);

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


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

done_get_content(unchanged, Ctx, St) ->
    send_result_reply(get_content, 204, undefined, Ctx, St);

done_get_content(#erod_content{type = entity} = Result, Ctx, St) ->
    send_result_reply(get_content, 200, Result, Ctx, St);

done_get_content(#erod_content{type = patch} = Result, Ctx, St) ->
    send_result_reply(get_content, 206, Result, Ctx, St).


done_get_children(unchanged, Ctx, St) ->
    send_result_reply(get_children, 204, undefined, Ctx, St);

done_get_children(#erod_page{type = entity} = Result, Ctx, St) ->
    send_result_reply(get_children, 200, Result, Ctx, St);

done_get_children(#erod_page{type = patch} = Result, Ctx, St) ->
    send_result_reply(get_children, 206, Result, Ctx, St).


send_result_reply(Act, Status, _Res,  Ctx, #?St{replied = true} = St) ->
    lager:warning("Reply ~w for action ~p already sent to ~s.",
                  [Status, Act, erod_context:log_id(Ctx)]),
    St;

send_result_reply(_, Status, Result, _, St) ->
    #?St{conn = Conn, cls = Cls, id = Id} = St,
    erodws_connection:send_result_reply(Conn, Cls, Id, Status, Result),
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
