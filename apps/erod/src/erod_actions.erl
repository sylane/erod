-module(erod_actions).

-include("erod_context.hrl").

-export([perform/1,
         perform/2,
         perform/3]).


perform(#?Ctx{actions = undefined}) -> ok;

perform(#?Ctx{actions = []}) -> ok;

perform(#?Ctx{actions = [{Action, Args} |_]} = Ctx) ->
    perform_action(Action, Args, Ctx).


perform(Actions, #?Ctx{actions = undefined} = Ctx) ->
    perform(Ctx#?Ctx{actions = Actions}).


perform(Action, Args, #?Ctx{actions = undefined} = Ctx) ->
    perform(Ctx#?Ctx{actions = [{Action, Args}]}).



perform_action(get_content, _, #?Ctx{policy = undefined} = Ctx) ->
    erod_context:warning("Cannot get_content, not yet authenticated.", [], Ctx),
    erod_context:failed(not_authenticated, Ctx);

perform_action(get_content, Args, Ctx) ->
    erod_registry:perform(get_content, Args, Ctx);

perform_action(get_children, _, #?Ctx{policy = undefined} = Ctx) ->
    erod_context:warning("Cannot get_children, not yet authenticated.", [], Ctx),
    erod_context:failed(not_authenticated, Ctx);

perform_action(get_children, Args, Ctx) ->
    erod_registry:perform(get_children, Args, Ctx);

perform_action(login, {Identity, Credential}, #?Ctx{sess = undefined} = Ctx) ->
    try
        case erod_user_manager:get_user(Identity) of
            {error, Reason} ->
                erod_context:debug("User ~p not found.", [Identity], Ctx),
                erod_context:failed(Reason, Ctx);
            {ok, User} ->
                erod_user:perform(User, login, Credential, Ctx)
        end
    catch
        _:Error2 ->
            erod_context:debug("Login failed: ~p.", [Error2], Ctx),
            erod_context:failed(Error2, Ctx)
    end;

perform_action(login, _, Ctx) ->
    erod_context:warning("Cannot login, already authenticated.", [], Ctx),
    erod_context:failed(already_authenticated, Ctx);

perform_action(restore, {Identity, Token}, #?Ctx{sess = undefined} = Ctx) ->
    try
        case erod_session_manager:find_session(Token) of
            {error, Reason} ->
                erod_context:debug("Session ~p not found.", [Token], Ctx),
                erod_context:failed(Reason, Ctx);
            {ok, Session} ->
                erod_session:perform(Session, restore, Identity, Ctx)
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
