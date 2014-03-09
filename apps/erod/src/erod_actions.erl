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
%%% @doc TODO: Document module erod_actions.
%%% @end
%%% ==========================================================================

-module(erod_actions).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_context.hrl").


%%% ==========================================================================
%%% Eports
%%% ==========================================================================

%%% API functions
-export([perform/1,
         perform/2,
         perform/3]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Performs the next action for the given context.
%% @end
%% -----------------------------------------------------------------
-spec perform(Context) -> ok
    when Context :: erod:context().
%% -----------------------------------------------------------------

perform(#?Ctx{actions = undefined}) -> ok;

perform(#?Ctx{actions = []}) -> ok;

perform(#?Ctx{actions = [{Action, Args} |_]} = Ctx) ->
    perform_action(Action, Args, Ctx).


%% -----------------------------------------------------------------
%% @doc Setups the given context with the specified list of actions
%% and starts the first one. If the context already have some actions
%% setup the function will crash.
%% @end
%% -----------------------------------------------------------------
-spec perform(Actions, Context) -> ok
    when Actions :: erod:actions(), Context :: erod:context().
%% -----------------------------------------------------------------

perform(Actions, #?Ctx{actions = undefined} = Ctx) ->
    perform(Ctx#?Ctx{actions = Actions}).


%% -----------------------------------------------------------------
%% @doc Setups the given context with the specified action and starts it.
%% If the context already have some actions setup the function will crash.
%% @end
%% -----------------------------------------------------------------
-spec perform(Action, Args, Context) -> ok
    when Action :: erod:action_id(), Args :: erod:action_args(),
         Context :: erod:context().
%% -----------------------------------------------------------------

perform(Action, Args, #?Ctx{actions = undefined} = Ctx) ->
    perform(Ctx#?Ctx{actions = [{Action, Args}]}).


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

perform_action(get_content, Args, Ctx) ->
    perform_registry_action(get_content, Args, Ctx);

perform_action(get_children, Args, Ctx) ->
    perform_registry_action(get_children, Args, Ctx);

perform_action(patch_content, Args, Ctx) ->
    perform_registry_action(patch_content, Args, Ctx);

perform_action(login, [Identity |_] = Args, #?Ctx{sess = undefined} = Ctx) ->
    try
        case erod_user_manager:get_user(Identity) of
            {error, Reason} ->
                erod_context:debug("User ~p not found.", [Identity], Ctx),
                erod_context:failed(Reason, Ctx);
            {ok, User} ->
                erod_user:perform(User, login, Args, Ctx)
        end
    catch
        _:Error2 ->
            erod_context:debug("Login failed: ~p.", [Error2], Ctx),
            erod_context:failed(Error2, Ctx)
    end;

perform_action(login, _, Ctx) ->
    erod_context:warning("Cannot login, already authenticated.", [], Ctx),
    erod_context:failed(already_authenticated, Ctx);

perform_action(restore, [_, Token |_] = Args, #?Ctx{sess = undefined} = Ctx) ->
    try
        case erod_session_manager:find_session(Token) of
            {error, Reason} ->
                erod_context:debug("Session ~p not found.", [Token], Ctx),
                erod_context:failed(Reason, Ctx);
            {ok, Session} ->
                erod_session:perform(Session, restore, Args, Ctx)
        end
    catch
        _:Error2 ->
            erod_context:debug("Restore failed: ~p.", [Error2], Ctx),
            erod_context:failed(Error2, Ctx)
    end;

perform_action(restore, _, Ctx) ->
    erod_context:warning("Cannot restore, already authenticated.", [], Ctx),
    erod_context:failed(already_authenticated, Ctx);

perform_action(bind, _, #?Ctx{sess = undefined} = Ctx) ->
    erod_context:warning("Cannot bind, not yet authenticated.", [], Ctx),
    erod_context:failed(not_authenticated, Ctx);

perform_action(bind, Args, #?Ctx{sess = Session} = Ctx) ->
    erod_session:perform(Session, bind, Args, Ctx);

perform_action(logout, _, #?Ctx{user = undefined} = Ctx) ->
    erod_context:warning("Cannot logout, not yet authenticated.", [], Ctx),
    erod_context:failed(not_authenticated, Ctx);

perform_action(logout, Args, #?Ctx{user = User} = Ctx) ->
    erod_user:perform(User, logout, Args, Ctx);

perform_action(Action, Args, Ctx) ->
    erod_context:error("Cannot perform unknown action ~p with arguments ~p.",
                       [Action, Args], Ctx),
    erod_context:failed(unknown_action, Ctx).


perform_registry_action(Action, _, #?Ctx{policy = undefined} = Ctx) ->
    erod_context:warning("Cannot perform ~w action, "
                         "not yet authenticated.", [Action], Ctx),
    erod_context:failed(not_authenticated, Ctx);

perform_registry_action(Action, Args, Ctx) ->
    erod_registry:perform(Action, Args, Ctx).
