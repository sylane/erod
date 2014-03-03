-module(erod_context).

-include("erod_policy.hrl").
-include("erod_context.hrl").


-export([new/2,
         compact/1,
         clone/3,
         log_id/1,
         done/1,
         done/2,
         failed/2]).

-export([debug/3,
         info/3,
         warning/3,
         error/3]).


-export(['_attach'/6,
         '_release'/1]).

-type context() :: #?Ctx{}.
-export_type([context/0]).


-callback init(Options :: term()) ->
    State :: term().

-callback format_log_id(LogId, State)
    -> LogId
    when LogId :: binary(), State :: term().

-callback done(Action, Result, Ctx, State)
    -> State
    when Action :: atom(), Result :: term(),
         Ctx :: erod:context(), State :: term().

-callback failed(Action, Error, Ctx, State)
    -> State
    when Action :: atom(), Error :: term(),
         Ctx :: erod:context(), State :: term().

-callback attached(Ctx, State)
    -> State
    when Ctx :: erod:context(), State :: term().

-callback released(State)
    -> State
    when State :: term().


-spec new(Module, Options)
    -> Context
    when Module :: module(), Options :: term(), Context :: erod:context().

new(Module, Options) ->
    update_log_id(#?Ctx{mod = Module, sub = Module:init(Options)}).


-spec compact(Context)
    -> Context
    when Context :: erod:context().

compact(Ctx) ->
    Ctx#?Ctx{actions = undefined, mod = undefined, sub = undefined}.


-spec clone(Module, Options, Context)
    -> Context
    when Module :: module(), Options :: term(), Context :: erod:context().

clone(Module, Options, Ctx) ->
    Ctx#?Ctx{actions = undefined, mod = Module, sub = Module:init(Options)}.


-spec log_id(Context)
    -> LogId
    when Context :: erod:context(), LogId :: binary().

log_id(#?Ctx{log_id = LogId}) -> LogId.


-spec done(Context)
    -> ok
    when Context :: erod:context().

done(Ctx) -> done(undefined, Ctx).


-spec done(Result, Context)
    -> ok
    when Result :: term(), Context :: erod:context().

done(Result, #?Ctx{actions = [{_, _} |_]} = Ctx) ->
    #?Ctx{actions = [{Action, _} |Actions], mod = Mod, sub = Sub} = Ctx,
    NewSub = Mod:done(Action, Result, Ctx, Sub),
    NewCtx = Ctx#?Ctx{actions = Actions, sub = NewSub},
    erod_actions:perform(NewCtx);

done(Result, #?Ctx{mod = Mod, sub = Sub} = Ctx) ->
    _ = Mod:done(undefined, Result, Ctx, Sub),
    ok.


-spec failed(Reason, Context)
    -> ok
    when Reason :: term(), Context :: erod:context().

failed(Reason, #?Ctx{actions = [{_, _} |_]} = Ctx) ->
    #?Ctx{actions = [{Action, _} |_], mod = Mod, sub = Sub} = Ctx,
    _ = Mod:failed(Action, Reason, Ctx, Sub),
    ok;

failed(Reason, #?Ctx{mod = Mod, sub = Sub} = Ctx) ->
    _ = Mod:failed(undefined, Reason, Ctx, Sub),
    ok.


-spec debug(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: list(), Context :: erod:context().

debug(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:debug("~s " ++ Format, [LogId |Params]).


-spec info(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: list(), Context :: erod:context().

info(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:info("~s " ++ Format, [LogId |Params]).


-spec warning(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: list(), Context :: erod:context().

warning(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:warning("~s " ++ Format, [LogId |Params]).


-spec error(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: list(), Context :: erod:context().

error(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:error("~s " ++ Format, [LogId |Params]).


-spec '_attach'(UserId, UserPid, SessId, SessPid, Policy, Context)
    -> Context
    when UserId :: erod:user_id(), UserPid :: pid(),
         SessId :: erod:session_id(), SessPid :: pid(),
         Policy :: erod:policy(), Context :: erod:context().

'_attach'(UserId, UserPid, SessId, SessPid, Policy, Ctx) ->
    #?Ctx{mod = Mod, sub = Sub} = Ctx,
    NewCtx = update_log_id(Ctx#?Ctx{user_id = UserId, user = UserPid,
                                    sess_id = SessId, sess = SessPid,
                                    policy = Policy}),
    NewCtx#?Ctx{sub = Mod:attached(NewCtx, Sub)}.


-spec '_release'(Context)
    -> Context
    when Context :: erod:context().

'_release'(Ctx) ->
    #?Ctx{mod = Mod, sub = Sub} = Ctx,
    NewCtx = update_log_id(Ctx#?Ctx{user_id = undefined, user = undefined,
                                    sess_id = undefined, sess = undefined}),
    NewCtx#?Ctx{sub = Mod:released(Sub)}.



update_log_id(Ctx) ->
    #?Ctx{user_id = UserId, sess_id = SessId, mod = Mod, sub = Sub} = Ctx,
    LogId = Mod:format_log_id(format_log_id(UserId, SessId), Sub),
    Ctx#?Ctx{log_id = <<"<", LogId/binary, ">">>}.



format_log_id(UID, SID) when is_integer(UID), is_integer(SID) ->
    UIDBin = integer_to_binary(UID),
    SIDBin = integer_to_binary(SID),
    <<UIDBin/binary, ",", SIDBin/binary>>;

format_log_id(UID, _) when is_integer(UID) ->
    UIDBin = integer_to_binary(UID),
    <<UIDBin/binary, ",?">>;

format_log_id(_, _) ->
    <<"?,?">>.
