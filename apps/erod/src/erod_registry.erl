-module(erod_registry).

-behaviour(gen_server).

-export([start_link/0]).

-export([register_document/2,
         unregister_document/1, unregister_document/2,
         find_document/1,
         get_document/1,
         get_watchers/1,
         add_watcher/2,
         del_watcher/1, del_watcher/2,
         get_content/1, get_content/2, get_content/3,
         get_children/3, get_children/4, get_children/5,
         notify_change/2]).

-export([perform/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PROCESS, ?MODULE).
-define(St, ?MODULE).
-define(WATCHER_KEY_TO_PID, erod_watcher_key_to_pid).
-define(WATCHER_PID_TO_KEY, erod_watcher_pid_to_key).
-define(DOCUMENT_KEY_TO_PID, erod_document_key_to_pid).
-define(DOCUMENT_PID_TO_KEY, erod_document_pid_to_key).


-record(?St, {factories}).


start_link() ->
    gen_server:start_link({local, ?PROCESS}, ?MODULE, [], []).


register_document(DocKey, DocPid) when is_pid(DocPid) ->
    gen_server:cast(?PROCESS, {link_document, DocPid}),
    Item = {DocKey, DocPid},
    ets:insert(?DOCUMENT_PID_TO_KEY, Item),
    ets:insert(?DOCUMENT_KEY_TO_PID, Item),
    ok.


unregister_document(DocKey) ->
    unregister_document_impl(DocKey, unregistered).


unregister_document(DocKeys, DocPid) when is_list(DocKeys), is_pid(DocPid) ->
    unregister_documents_impl(DocKeys, DocPid, unregistered);

unregister_document(DocKey, DocPid) when is_pid(DocPid) ->
    unregister_document_impl(DocKey, DocPid, unregistered).


find_document(DocKey) ->
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> {ok, DocPid}
    catch error:badarg -> {error, document_not_found} end.


get_document(DocKey) ->
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> {ok, DocPid}
    catch error:badarg ->
        gen_server:call(?PROCESS, {get_document, DocKey})
    end.


get_watchers(DocKey) ->
    ets:lookup_element(?WATCHER_KEY_TO_PID, DocKey, 2).


add_watcher(_DocKey, undefined) -> ok;

add_watcher(DocKey, WatcherPid) when is_pid(WatcherPid) ->
    gen_server:cast(?PROCESS, {link_document, WatcherPid}),
    Item = {DocKey, WatcherPid},
    ets:insert(?WATCHER_KEY_TO_PID, Item),
    ets:insert(?WATCHER_PID_TO_KEY, Item),
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid ->
            erod_document_process:add_watcher(DocPid, DocKey, WatcherPid)
    catch error:badarg -> ok end.


del_watcher(undefined) -> ok;

del_watcher(WatcherPid) when is_pid(WatcherPid) ->
    % No registration must be done for this watcher in parallele
    gen_server:cast(?PROCESS, {unlink_document, WatcherPid}),
    unregister_interests_impl(WatcherPid).


del_watcher(_DocKeys, undefined) -> ok;

del_watcher(DocKeys, WatcherPid) when is_list(DocKeys) ->
    unregister_interests_impl(DocKeys, WatcherPid);

del_watcher(DocKey, WatcherPid) ->
    unregister_interest_impl(DocKey, WatcherPid).


get_content(DocKey) ->
    get_content(DocKey, undefined, undefined).


get_content(DocKey, FromVer) ->
    get_content(DocKey, FromVer, undefined).


get_content(DocKey, FromVer, Watcher) ->
    add_watcher(DocKey, Watcher),
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        Pid -> erod_document_process:get_content(Pid, DocKey, FromVer, Watcher)
    catch error:badarg ->
        case gen_server:call(?PROCESS, {get_document_for_content, DocKey}) of
            {error, _Reason} = Error -> Error;
            {ask_factory, Factory} ->
                erod_factory:get_content(DocKey, Factory);
            {ok, Pid} ->
                erod_document_process:get_content(Pid, DocKey, FromVer, Watcher)
        end
    end.


get_children(DocKey, ViewId, PageId) ->
    get_children(DocKey, ViewId, PageId, undefined, undefined).


get_children(DocKey, ViewId, PageId, FromVer) ->
    get_children(DocKey, ViewId, PageId, FromVer, undefined).


get_children(DocKey, ViewId, PageId, FromVer, Watcher)->
    add_watcher(DocKey, Watcher),
    case get_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_children(DocPid, DocKey, ViewId,
                                               PageId, FromVer, Watcher)
    end.


notify_change(DocKey, Patch) ->
    try ets:lookup_element(?WATCHER_KEY_TO_PID, DocKey, 2) of
        Watchers -> notify_change_impl(Watchers, DocKey, Patch)
    catch error:badarg -> ok end.


perform(Action, Args, Ctx) ->
    perform_outside_action(Action, Args, Ctx).


init([]) ->
    lager:info("Registry process started.", []),
    process_flag(trap_exit, true),
    Factories = load_factories(),
    _ = ets:new(?WATCHER_KEY_TO_PID, [named_table, public, bag, {keypos, 1}]),
    _ = ets:new(?WATCHER_PID_TO_KEY, [named_table, public, bag, {keypos, 2}]),
    _ = ets:new(?DOCUMENT_KEY_TO_PID, [named_table, public, set, {keypos, 1}]),
    _ = ets:new(?DOCUMENT_PID_TO_KEY, [named_table, public, bag, {keypos, 2}]),
    {ok, #?St{factories = Factories}}.


handle_call({get_document_for_content, DocKey}, From, State) ->
    Cont = fun(Result) -> gen_server:reply(From, Result) end,
    {noreply, get_document_for_content(DocKey, Cont, State)};

handle_call({get_document, DocKey}, From, State) ->
    Cont = fun(Result) -> gen_server:reply(From, Result) end,
    {noreply, lookup_or_create(DocKey, Cont, State)};

handle_call(Request, {From, _Ref}, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast({perform, Action, Args, Ctx}, State) ->
    {noreply, perform_inside_action(Action, Args, Ctx, State)};

handle_cast({link_document, Pid}, State) ->
    catch link(Pid),
    {noreply, State};

handle_cast({unlink_document, Pid}, State) ->
    catch unlink(Pid),
    {noreply, State};

handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info({'EXIT', Pid, _Reason}, State) ->
    lager:debug("Registry's document ~p died, cleaning up.", [Pid]),
    unregister_interests_impl(Pid),
    unregister_document_impl(Pid, dead),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:info("Registry process terminated: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



load_factories() ->
    {ok, AppName} = application:get_application(),
    FactorySpecs = application:get_env(AppName, document_factories, []),
    erodlib_maps:from_items([{T, F} || {T, M, O} <- FactorySpecs,
                          begin {ok, F} = erod_factory:new(M, O), true end]).


get_factory({Type, _Id}, #?St{factories = Factories}) ->
    case erodlib_maps:lookup(Type, Factories) of
        {value, Fac} -> Fac;
        none -> none
    end.


lookup_or_create(DocKey, Cont, State) ->
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> Cont({ok, DocPid}), State
    catch
        error:badarg -> create_document(DocKey, Cont, State)
    end.


create_document(DocKey, Cont, State) ->
    case get_factory(DocKey, State) of
        none -> Cont({error, document_not_found}), State;
        Factory ->
            % The documents should be hable to query other documents in init.
            _ = spawn(fun() -> spawn_document(DocKey, Cont , Factory) end),
            State
    end.

%% Run in its own process
spawn_document(DocKey, Cont, Factory) ->
    case erod_factory:start_document(DocKey, Factory) of
        {ok, DocPid} -> Cont({ok, DocPid});
        {error, Reason} -> Cont({error, Reason})
    end.


get_document_for_content(DocKey, Cont, State) ->
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> Cont({ok, DocPid}), State
    catch
        error:badarg ->
            case get_factory(DocKey, State) of
                none -> create_document(DocKey, Cont, State);
                Fac ->
                    case erod_factory:knows_content(DocKey, Fac) of
                        true -> Cont({ask_factory, Fac}), State;
                        false -> create_document(DocKey, Cont, State)
                    end
            end
    end.


unregister_document_impl(DocPid, Reason) when is_pid(DocPid) ->
    try ets:lookup_element(?DOCUMENT_PID_TO_KEY, DocPid, 1) of
        DocKeys -> unregister_documents_impl(DocKeys, DocPid, Reason)
    catch error:badarg -> ok end;

unregister_document_impl(DocKey, Reason) ->
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> unregister_document_impl(DocKey, DocPid, Reason)
    catch error:badarg -> ok end.


unregister_documents_impl([], _DocPid, _Reason) -> ok;

unregister_documents_impl([DocKey |DocKeys], DocPid, Reason) ->
    unregister_document_impl(DocKey, DocPid, Reason),
    unregister_documents_impl(DocKeys, DocPid, Reason).


unregister_document_impl(DocKey, DocPid, Reason) ->
    ets:delete_object(?DOCUMENT_PID_TO_KEY, {DocKey, DocPid}),
    ets:delete(?DOCUMENT_KEY_TO_PID, DocKey),
    try ets:lookup_element(?WATCHER_KEY_TO_PID, DocKey, 2) of
        Watchers -> notify_state_impl(Watchers, DocKey, Reason)
    catch error:badarg -> ok end.


unregister_interests_impl(WatcherPid) ->
    try ets:lookup_element(?WATCHER_PID_TO_KEY, WatcherPid, 1) of
        Keys -> del_watcher(Keys, WatcherPid)
    catch error:badarg -> ok end.


unregister_interests_impl([], _WatcherPid) -> ok;

unregister_interests_impl([DocKey |DocKeys], WatcherPid) ->
    unregister_interest_impl(DocKey, WatcherPid),
    unregister_interests_impl(DocKeys, WatcherPid).


unregister_interest_impl(DocKey, WatcherPid) ->
    Item = {DocKey, WatcherPid},
    ets:delete_object(?WATCHER_KEY_TO_PID, Item),
    ets:delete_object(?WATCHER_PID_TO_KEY, Item),
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> erod_document_process:del_watcher(DocPid, DocKey, WatcherPid)
    catch error:badarg -> ok end.


notify_change_impl([], _DocKey, _Patch) -> ok;

notify_change_impl([WatcherPid |Watchers], DocKey, Patch) ->
    erod_document_process:notify_change(WatcherPid, DocKey, Patch),
    notify_change_impl(Watchers, DocKey, Patch).


notify_state_impl([], _DocKey, _State) -> ok;

notify_state_impl([WatcherPid |Watchers], DocKey, State) ->
    erod_document_process:notify_state(WatcherPid, DocKey, State),
    notify_state_impl(Watchers, DocKey, State).


%% Running from inside the registry process
perform_inside_action(Action, [DocKey |_] = Args, Ctx, State) ->
   Cont = fun(R) -> continue_inside_action(R, Action, Args, Ctx) end,
   lookup_or_create(DocKey, Cont, State).


%% Running from inside the calling process or a specially spawned process
%% if case the document had to be created.
continue_inside_action({ok, DocPid}, Action, Args, Ctx) ->
    erod_document_process:perform(DocPid, Action, Args, Ctx);

continue_inside_action({error, Reason}, Action, [DocKey |_], Ctx) ->
    erod_context:warning("Registry could not find document ~p to perform ~p.",
                         [DocKey, Action], Ctx),
    erod_context:failed(Reason, Ctx).


%% Running from inside the calling process.
perform_outside_action(Action, [DocKey |_] = Args, Ctx)
  when Action =:= get_content; Action =:= get_children;
       Action =:= patch_content ->
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> erod_document_process:perform(DocPid, Action, Args, Ctx)
    catch error:badarg ->
        gen_server:cast(?PROCESS, {perform, Action, Args, Ctx})
    end;

perform_outside_action(Action, Args, Ctx) ->
    erod_context:error("Registry does't know how to perform action ~p with "
                       "arguments ~p.", [Action, Args], Ctx),
    erod_context:failed(unknown_action, Ctx),
    ok.
