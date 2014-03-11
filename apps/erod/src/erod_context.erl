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
%%% @doc TODO: Document module erod.
%%% @end
%%% ==========================================================================

-module(erod_context).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_policy.hrl").
-include("erod_context.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/2,
         compact/1,
         clone/3,
         log_id/1,
         done/1,
         done/2,
         failed/2]).

%%% Logging functions
-export([debug/3,
         info/3,
         warning/3,
         error/3]).

%%% Internal exported functions
-export(['_attach'/6,
         '_release'/1]).

%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type context() :: #?Ctx{}.
-export_type([context/0]).


%%% ==========================================================================
%%% Behaviour erod_context Specification
%%% ==========================================================================

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


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new context with given callback module and options.
%% @end
%% -----------------------------------------------------------------
-spec new(Module, Options)
    -> Context
    when Module :: module(), Options :: term(), Context :: erod:context().
%% -----------------------------------------------------------------

new(Module, Options) ->
    update_log_id(#?Ctx{mod = Module, sub = Module:init(Options)}).


%% -----------------------------------------------------------------
%% @doc Compacts a context keeping only the minimum amount of information
%% to be able to clone a new context from it.
%% Used by protocols when attaching a context to remove unnecessary data.
%% @end
%% -----------------------------------------------------------------
-spec compact(Context)
    -> Context
    when Context :: erod:context().
%% -----------------------------------------------------------------

compact(Ctx) ->
    Ctx#?Ctx{actions = undefined, mod = undefined, sub = undefined}.


%% -----------------------------------------------------------------
%% @doc Clones a context from another one keeping the session,
%% user and policy information.
%% @end
%% -----------------------------------------------------------------
-spec clone(Module, Options, Context)
    -> Context
    when Module :: module(), Options :: term(), Context :: erod:context().
%% -----------------------------------------------------------------

clone(Module, Options, Ctx) ->
    Ctx#?Ctx{actions = undefined, mod = Module, sub = Module:init(Options)}.


%% -----------------------------------------------------------------
%% @doc Gives the logging identifier of the context.
%% @end
%% -----------------------------------------------------------------
-spec log_id(Context)
    -> LogId
    when Context :: erod:context(), LogId :: binary().
%% -----------------------------------------------------------------

log_id(#?Ctx{log_id = LogId}) -> LogId.


%% -----------------------------------------------------------------
%% @doc Marks the context's current action as done and starts the next one.
%% @end
%% -----------------------------------------------------------------
-spec done(Context)
    -> ok
    when Context :: erod:context().
%% -----------------------------------------------------------------

done(Ctx) -> done(undefined, Ctx).


%% -----------------------------------------------------------------
%% @doc Marks the context's current action as done with specified result
%% and starts the next one.
%% @end
%% -----------------------------------------------------------------
-spec done(Result, Context)
    -> ok
    when Result :: term(), Context :: erod:context().
%% -----------------------------------------------------------------

done(Result, #?Ctx{actions = [{_, _} |_]} = Ctx) ->
    #?Ctx{actions = [{Action, _} |Actions], mod = Mod, sub = Sub} = Ctx,
    NewSub = Mod:done(Action, Result, Ctx, Sub),
    NewCtx = Ctx#?Ctx{actions = Actions, sub = NewSub},
    erod_actions:perform(NewCtx);

done(Result, #?Ctx{mod = Mod, sub = Sub} = Ctx) ->
    _ = Mod:done(undefined, Result, Ctx, Sub),
    ok.


%% -----------------------------------------------------------------
%% @doc Marks the context's current action as failed for the given reason.
%% @end
%% -----------------------------------------------------------------
-spec failed(Reason, Context)
    -> ok
    when Reason :: term(), Context :: erod:context().
%% -----------------------------------------------------------------

failed(Reason, #?Ctx{actions = [{_, _} |_]} = Ctx) ->
    #?Ctx{actions = [{Action, _} |_], mod = Mod, sub = Sub} = Ctx,
    _ = Mod:failed(Action, Reason, Ctx, Sub),
    ok;

failed(Reason, #?Ctx{mod = Mod, sub = Sub} = Ctx) ->
    _ = Mod:failed(undefined, Reason, Ctx, Sub),
    ok.


%%% ==========================================================================
%%% Logging Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Same as lager:debug/2 with the contet log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec debug(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: [term()] | [], Context :: erod:context().
%% -----------------------------------------------------------------

debug(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:debug("~s " ++ Format, [LogId |Params]).


%% -----------------------------------------------------------------
%% @doc Same as lager:info/2 with the contet log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec info(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: [term()] | [], Context :: erod:context().
%% -----------------------------------------------------------------

info(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:info("~s " ++ Format, [LogId |Params]).


%% -----------------------------------------------------------------
%% @doc Same as lager:warning/2 with the contet log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec warning(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: [term()] | [], Context :: erod:context().
%% -----------------------------------------------------------------

warning(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:warning("~s " ++ Format, [LogId |Params]).


%% -----------------------------------------------------------------
%% @doc Same as lager:error/2 with the contet log identifier prefixed.
%% @end
%% -----------------------------------------------------------------
-spec error(Format, Params, Context)
    -> ok
    when Format :: string(), Params :: [term()] | [], Context :: erod:context().
%% -----------------------------------------------------------------

error(Format, Params, #?Ctx{log_id = LogId}) ->
    lager:error("~s " ++ Format, [LogId |Params]).


%%% ==========================================================================
%%% Internal Exported Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Attaches the context to a user and a session giving a security
%% policy. Protocols can then clone the context to perform actions
%% that need authentication without authenticating again.
%% @end
%% @private
%% -----------------------------------------------------------------
-spec '_attach'(UserId, UserPid, SessId, SessPid, Policy, Context) -> Context
    when UserId :: erod:user_id(), UserPid :: pid(),
         SessId :: erod:session_id(), SessPid :: pid(),
         Policy :: erod:policy(), Context :: erod:context().
%% -----------------------------------------------------------------

'_attach'(UserId, UserPid, SessId, SessPid, Policy, Ctx) ->
    #?Ctx{mod = Mod, sub = Sub} = Ctx,
    NewCtx = update_log_id(Ctx#?Ctx{user_id = UserId, user = UserPid,
                                    sess_id = SessId, sess = SessPid,
                                    policy = Policy}),
    NewCtx#?Ctx{sub = Mod:attached(NewCtx, Sub)}.


%% -----------------------------------------------------------------
%% @doc Releases a context, protocols MUST stop using it to perform actions.
%% @end
%% @private
%% -----------------------------------------------------------------
-spec '_release'(Context) -> Context
    when Context :: erod:context().
%% -----------------------------------------------------------------

'_release'(Ctx) ->
    #?Ctx{mod = Mod, sub = Sub} = Ctx,
    NewCtx = update_log_id(Ctx#?Ctx{user_id = undefined, user = undefined,
                                    sess_id = undefined, sess = undefined}),
    NewCtx#?Ctx{sub = Mod:released(Sub)}.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

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
