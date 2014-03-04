-module(erod_proxy).

-include("erod_context.hrl").

-export([new/2,
         accept/2,
         handle_message/2,
         notify/4]).

-export([debug/3,
         info/3,
         warning/3,
         error/3]).


-define(Proxy, ?MODULE).
-record(?Proxy, {log_id :: binary() | undefined,
                 mod :: module(),
                 sub :: term()}).

-type proxy() :: #?Proxy{}.
-export_type([proxy/0]).


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


new(Module, Options) ->
    #?Proxy{mod = Module, sub = Module:init(Options)}.


accept(#?Ctx{log_id = LogId} = Ctx, #?Proxy{mod = Mod, sub = Sub} = Proxy) ->
    NewProxy = Proxy#?Proxy{log_id = LogId},
    case Mod:accept(Ctx, Proxy, Sub) of
        {ok, NewSub} ->
            {ok, NewProxy#?Proxy{sub = NewSub}};
        {error, Reason, NewSub} ->
            {error, Reason, Proxy#?Proxy{sub = NewSub}}
    end.


handle_message(Message, #?Proxy{mod = Mod, sub = Sub} = Proxy) ->
    case Mod:handle_message(Message, Proxy, Sub) of
        ignored -> ignored;
        {ok, NewSub} -> {ok, Proxy#?Proxy{sub = NewSub}};
        {dead, Reason, NewSub} ->
            {dead, Reason, Proxy#?Proxy{sub = NewSub}}
    end.


notify(Name, Fmt, Data, #?Proxy{mod = Mod, sub = Sub} = Proxy) ->
    Proxy#?Proxy{sub = Mod:notify(Name, Fmt, Data, Proxy, Sub)}.


-spec debug(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().

debug(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:debug(Format, Params);

debug(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:debug("~s " ++ Format, [LogId |Params]).


-spec info(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().

info(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:info(Format, Params);

info(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:info("~s " ++ Format, [LogId |Params]).


-spec warning(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().

warning(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:warning(Format, Params);

warning(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:warning("~s " ++ Format, [LogId |Params]).


-spec error(Format, Params, Proxy)
    -> ok
    when Format :: string(), Params :: list(), Proxy :: erod:proxy().

error(Format, Params, #?Proxy{log_id = undefined}) ->
    lager:error(Format, Params);

error(Format, Params, #?Proxy{log_id = LogId}) ->
    lager:error("~s " ++ Format, [LogId |Params]).
