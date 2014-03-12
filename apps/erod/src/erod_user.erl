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
%%% @doc TODO: Document module erod_user.
%%% @end
%%% @private
%%% ==========================================================================

-module(erod_user).

-author('Sebastien Merle').

-behaviour(gen_server).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_context.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control functions
-export([start_link/1]).

%%% Internal API functions
-export([perform/4]).

%%% Behaiour gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {user_id :: pos_integer(),
              mod :: module(),
              sub :: term(),
              sessions = [] :: sess_list()}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type sess_entry() :: {Id :: pos_integer(), Sess :: pid()}.
-type sess_list() :: list(sess_entry()) | [].


%%% ==========================================================================
%%% Behaviour erod_user Specifications
%%% ==========================================================================

-callback init(Identity, Options)
    -> {stop, Reason}
     | {ok, UserId, State}
    when Identity :: term(), Options :: term(),
         UserId :: pos_integer(), State :: term(), Reason :: term().

-callback info(SessionToken, State)
    -> {ok, UserInfo, State}
    when SessionToken :: binary(), State :: term(), UserInfo :: term().

-callback authenticate(Credential, State)
    -> {error, Reason, State}
     | {ok, Policy, State}
    when Credential :: term(), State :: term(),
         Policy :: erod:policy(), Reason :: term().


%%% ==========================================================================
%%% Process Control Funtions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts and links a user process with specified identity.
%% @end
%% -----------------------------------------------------------------
-spec start_link(UserIdentity) -> {ok, UserPid} | {error, Reason}
    when UserIdentity :: term(), UserPid :: pid(),
         Reason :: unknown_user | term().
%% -----------------------------------------------------------------

start_link(UserIdent) ->
    gen_server:start_link(?MODULE, [UserIdent], []).


%%% ==========================================================================
%%% Internal API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Performs given action.
%% @end
%% -----------------------------------------------------------------
-spec perform(UserPid, Action, Args, Context) -> ok
    when UserPid :: pid(), Action :: erod:action_id(),
         Args :: erod:action_args(), Context :: erod:context().
%% -----------------------------------------------------------------

perform(User, Action, Args, Ctx) ->
    gen_server:cast(User, {perform, Action, Args, Ctx}).


%%% ==========================================================================
%%% Behaviour gen_server Callacks
%%% ==========================================================================

init([UserIdent]) ->
    process_flag(trap_exit, true),
    {ok, AppName} = application:get_application(),
    {ok, {Mod, Opts}} = application:get_env(AppName, user_mod),
    case Mod:init(UserIdent, Opts) of
        {stop, Reason} -> {stop, Reason};
        {ok, Id, Sub} ->
            lager:info("User ~p with identity ~p started.", [Id, UserIdent]),
            {ok, #?St{user_id = Id, mod = Mod, sub = Sub}}
    end.


handle_call(Request, {From, _Ref}, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast({perform, Action, Args, Ctx}, State) ->
    {noreply, perform_action(Action, Args, Ctx, State)};

handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info({'EXIT', Pid, Reason}, State) ->
    case del_session(Pid, State) of
        false -> {noreply, State};
        {true, {Id, Pid}, NewState} ->
            #?St{user_id = UserId} = State,
            lager:debug("User ~p's session ~p died: ~p.",
                        [UserId, Id, Reason]),
            %TODO: Do some session managment
            {noreply, NewState}
    end;

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, State) ->
    #?St{user_id = UserId} = State,
    lager:info("User ~p terminated: ~p", [UserId, Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

del_session(Pid, #?St{sessions = List} = State) when is_pid(Pid) ->
    case lists:keytake(Pid, 2, List) of
        false -> false;
        {value, Value, NewList} ->
            {true, Value, State#?St{sessions = NewList}}
    end;

del_session(SessId, #?St{sessions = List} = State) when is_integer(SessId) ->
    case lists:keytake(SessId, 1, List) of
        false -> false;
        {value, Value, NewList} ->
            {true, Value, State#?St{sessions = NewList}}
    end.


%%%TODO: Enable if needed or remove.
%% has_session(Pid, #?St{sessions = List}) when is_pid(Pid) ->
%%     case lists:keysearch(Pid, 2, List) of
%%         {value, Value} -> {true, Value};
%%         false -> false
%%     end;

has_session(SessId, #?St{sessions = List}) when is_integer(SessId) ->
    case lists:keysearch(SessId, 1, List) of
        {value, Value} -> {true, Value};
        false -> false
    end.


add_session(SessId, Pid, #?St{sessions = List} = State) ->
    State#?St{sessions = [{SessId, Pid} |List]}.


%%% --------------------------------------------------------------------------
%%% Action Handling
%%% --------------------------------------------------------------------------

perform_action(login, [_, Credential |_], Ctx, State) ->
    perform_login(Credential, Ctx, State);

perform_action(logout, _, Ctx, State) ->
    perform_logout(Ctx, State);

perform_action(Action, Args, Ctx, State) ->
    erod_context:error("User doesn't know how to perform action ~p with "
                       "arguments ~p.", [Action, Args], Ctx),
    erod_context:failed(unknown_action, Ctx),
    State.


perform_login(Cred, Ctx, State) ->
    #?St{mod = Mod, sub = Sub} = State,
    case Mod:authenticate(Cred, Sub) of
        {error, Reason, NewSub} ->
            login_failed(Reason, Ctx, State#?St{sub = NewSub});
        {ok, Policy, NewSub} ->
            login_create_session(Policy, Ctx, State#?St{sub = NewSub})
    end.


login_create_session(Policy, Ctx, State) ->
    #?St{user_id = UserId} = State,
    case erod_session_manager:new_session(UserId, self(), Policy) of
        {error, Reason} ->
            login_failed(Reason, Ctx, State);
        {ok, SessId, Sess, Token} ->
            login_setup(SessId, Sess, Token, Policy, Ctx, State)
    end.


login_setup(SID, Sess, Token, Pol, Ctx, State) ->
    try erlang:link(Sess) of
        true ->
            #?St{user_id = UID} = State,
            NewCtx = erod_context:'_attach'(UID, self(), SID, Sess, Pol, Ctx),
            login_succeed(Token, NewCtx, add_session(SID, Sess, State))
    catch
        error:badarg ->
            login_failed(session_died, Ctx, State)
    end.


login_succeed(Token, Ctx, State) ->
    erod_context:info("User login succeed.", [], Ctx),
    #?St{mod = Mod, sub = Sub} = State,
    {ok, UserInfo, NewSub} = Mod:info(Token, Sub),
    erod_context:done(UserInfo, Ctx),
    State#?St{sub = NewSub}.


login_failed(Reason, Ctx, State) ->
    erod_context:info("User login failed.", [], Ctx),
    erod_context:failed(Reason, Ctx),
    State.


perform_logout(#?Ctx{user_id = UID} = Ctx, #?St{user_id = UID} = State) ->
    #?Ctx{sess_id = SID, sess = Sess} = Ctx,
    case has_session(SID, State) of
        false ->
            logout_failed(session_not_found, Ctx, State);
        {true, {SID, Sess}} ->
            logout_close_session(Sess, Ctx, State)
    end;

perform_logout(Ctx, #?St{user_id = UID} = State) ->
    erod_context:error("User ~p received a logout request for another user.",
                       [UID], Ctx),
    logout_failed(internal_error, Ctx, State).


logout_close_session(Sess, Ctx, State) ->
    case erod_session:close(Sess, normal) of
        {error, Reason} ->
            logout_failed(Reason, Ctx, State);
        ok ->
            logout_succeed(Ctx, State)
    end.


logout_succeed(Ctx, State) ->
    erod_context:info("User logout succeed.", [], Ctx),
    erod_context:done(erod_context:'_release'(Ctx)),
    State.


logout_failed(Reason, Ctx, State) ->
    erod_context:failed(Reason, Ctx),
    State.
