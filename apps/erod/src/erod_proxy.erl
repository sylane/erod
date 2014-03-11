%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erod.
%%%
%%% Erod is free software: you can redistribute it and/or modify
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
%%% @doc TODO: Document module erod_proxy.
%%% @end
%%% ==========================================================================

-module(erod_proxy).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_context.hrl").


%%% ==========================================================================
%%% Export
%%% ==========================================================================

%%% API functions
-export([new/2,
         accept/2,
         handle_message/2,
         notify/4]).

%%% Logging functions
-export([debug/3,
         info/3,
         warning/3,
         error/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(Proxy, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?Proxy, {log_id :: binary() | undefined,
                 mod :: module(),
                 sub :: term()}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type proxy() :: #?Proxy{}.
-export_type([proxy/0]).


%%% ==========================================================================
%%% Behaviour erod_proxy Specifications
%%% ==========================================================================

-callback init(Options)
    -> State
    when Options :: term(), State :: term().

-callback accept(Ctx, Proxy, State)
    -> {ok, State}
     | {error, Reason, State}
    when Ctx :: erod:context(), Proxy :: erod:proxy(),
         State :: term(), Reason :: term().

-callback handle_message(Message, Proxy, State)
    -> ignored
     | {ok, State}
     | {dead, Reason, State}
    when Message :: term(), Proxy :: erod:proxy(),
         State :: term(), Reason :: term().

-callback notify(Name, Fmt, Data, Proxy, State)
    -> State
    when Name :: atom(), Fmt :: atom(), Data :: term(),
         Proxy :: erod:proxy(), State :: term().


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new proxy with specified callback module and options.
%% @end
%% -----------------------------------------------------------------
-spec new(Module, Options) -> Proxy
    when Module :: module(), Options :: term(), Proxy :: proxy().
%% -----------------------------------------------------------------

new(Module, Options) ->
    #?Proxy{mod = Module, sub = Module:init(Options)}.


%% -----------------------------------------------------------------
%% @doc Accepts the proxy.
%% Used by the session to accept a proxy, this mean the proxy will later
%% be used to send requests and notifications.
%% @end
%% @private
%% -----------------------------------------------------------------
-spec accept(Context, Proxy) -> {ok, Proxy} | {error, Reason, Proxy}
    when Context :: erod:context(), Proxy :: proxy(), Reason :: term().
%% -----------------------------------------------------------------

accept(#?Ctx{log_id = LogId} = Ctx, #?Proxy{mod = Mod, sub = Sub} = Proxy) ->
    NewProxy = Proxy#?Proxy{log_id = LogId},
    case Mod:accept(Ctx, Proxy, Sub) of
        {ok, NewSub} ->
            {ok, NewProxy#?Proxy{sub = NewSub}};
        {error, Reason, NewSub} ->
            {error, Reason, Proxy#?Proxy{sub = NewSub}}
    end.


%% -----------------------------------------------------------------
%% @doc Delegates message handling to the proxy.
%%
%% If the proxy's associated connection died the function will return
%% {dead, Reason, Proxy}, if it doesn't know what to do with the message
%% it will return 'ignored'.
%% @end
%% @private
%% -----------------------------------------------------------------
-spec handle_message(Message, Proxy)
    -> ignored | {ok, Proxy} | {error, Reason, Proxy}
    when Message :: term(), Proxy :: proxy(), Reason :: term().
%% -----------------------------------------------------------------

handle_message(Message, #?Proxy{mod = Mod, sub = Sub} = Proxy) ->
    case Mod:handle_message(Message, Proxy, Sub) of
        ignored -> ignored;
        {ok, NewSub} -> {ok, Proxy#?Proxy{sub = NewSub}};
        {dead, Reason, NewSub} ->
            {dead, Reason, Proxy#?Proxy{sub = NewSub}}
    end.


%% -----------------------------------------------------------------
%% @doc Sends a notification.
%% @end
%% @private
%% -----------------------------------------------------------------
-spec notify(Type, Format, Data, Proxy) -> Proxy
    when Type :: atom(), Format :: atom(), Data :: term(), Proxy :: proxy().
%% -----------------------------------------------------------------

notify(Type, Fmt, Data, #?Proxy{mod = Mod, sub = Sub} = Proxy) ->
    Proxy#?Proxy{sub = Mod:notify(Type, Fmt, Data, Proxy, Sub)}.



%%% ==========================================================================
%%% Logging Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Same as lager:debug/2 with the proxy log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec debug(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().
%% -----------------------------------------------------------------

debug(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:debug(Format, Params);

debug(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:debug("~s " ++ Format, [LogId |Params]).



%% -----------------------------------------------------------------
%% @doc Same as lager:info/2 with the proxy log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec info(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().
%% -----------------------------------------------------------------

info(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:info(Format, Params);

info(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:info("~s " ++ Format, [LogId |Params]).


%% -----------------------------------------------------------------
%% @doc Same as lager:warning/2 with the proxy log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec warning(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().
%% -----------------------------------------------------------------

warning(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:warning(Format, Params);

warning(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:warning("~s " ++ Format, [LogId |Params]).


%% -----------------------------------------------------------------
%% @doc Same as lager:error/2 with the proxy log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec error(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().
%% -----------------------------------------------------------------

error(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:error(Format, Params);

error(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:error("~s " ++ Format, [LogId |Params]).
