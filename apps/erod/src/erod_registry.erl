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
         find_content/1, find_content/2, find_content/3,
         find_children/3, find_children/4, find_children/5,
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
        gen_server:call(?PROCESS, {find_document, DocKey})
    end.


get_watchers(DocKey) ->
    ets:lookup_element(?WATCHER_KEY_TO_PID, DocKey, 2).


add_watcher(DocKey, WatcherPid) when is_pid(WatcherPid) ->
    gen_server:cast(?PROCESS, {link_document, WatcherPid}),
    Item = {DocKey, WatcherPid},
    ets:insert(?WATCHER_KEY_TO_PID, Item),
    ets:insert(?WATCHER_PID_TO_KEY, Item),
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid ->
            erod_document_process:add_watcher(DocPid, DocKey, WatcherPid)
    catch error:badarg -> ok end.


del_watcher(WatcherPid) when is_pid(WatcherPid) ->
    % No registration must be done for this watcher in parallele
    gen_server:cast(?PROCESS, {unlink_document, WatcherPid}),
    unregister_interests_impl(WatcherPid).


del_watcher(DocKeys, WatcherPid) when is_list(DocKeys) ->
    unregister_interests_impl(DocKeys, WatcherPid);

del_watcher(DocKey, WatcherPid) ->
    unregister_interest_impl(DocKey, WatcherPid).


find_content(DocKey) ->
    find_content(DocKey, undefined, undefined).


find_content(DocKey, FromVer) ->
    find_content(DocKey, FromVer, undefined).


find_content(DocKey, FromVer, undefined) ->
    case find_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_content(DocPid, DocKey, FromVer)
    end;

find_content(DocKey, FromVer, WatcherPid) when is_pid(WatcherPid) ->
    Item = {DocKey, WatcherPid},
    ets:insert(?WATCHER_KEY_TO_PID, Item),
    ets:insert(?WATCHER_PID_TO_KEY, Item),
    case find_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_content(DocPid, DocKey,
                                              FromVer, WatcherPid)
    end.


find_children(DocKey, ViewId, PageId) ->
    get_children(DocKey, ViewId, PageId, undefined, undefined).


find_children(DocKey, ViewId, PageId, FromVer) ->
    get_children(DocKey, ViewId, PageId, FromVer, undefined).


find_children(DocKey, ViewId, PageId, FromVer, undefined) ->
    case find_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_children(DocPid, DocKey, ViewId,
                                               PageId, FromVer)
    end;

find_children(DocKey, ViewId, PageId, FromVer, WatcherPid)
  when is_pid(WatcherPid) ->
    Item = {DocKey, WatcherPid},
    ets:insert(?WATCHER_KEY_TO_PID, Item),
    ets:insert(?WATCHER_PID_TO_KEY, Item),
    case find_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_children(DocPid, DocKey, ViewId,
                                               PageId, FromVer, WatcherPid)
    end.


get_content(DocKey) ->
    get_content(DocKey, undefined, undefined).


get_content(DocKey, FromVer) ->
    get_content(DocKey, FromVer, undefined).


get_content(DocKey, FromVer, undefined) ->
    case get_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_content(DocPid, DocKey, FromVer)
    end;

get_content(DocKey, FromVer, WatcherPid) when is_pid(WatcherPid) ->
    Item = {DocKey, WatcherPid},
    ets:insert(?WATCHER_KEY_TO_PID, Item),
    ets:insert(?WATCHER_PID_TO_KEY, Item),
    case get_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_content(DocPid, DocKey,
                                              FromVer, WatcherPid)
    end.


get_children(DocKey, ViewId, PageId) ->
    get_children(DocKey, ViewId, PageId, undefined, undefined).


get_children(DocKey, ViewId, PageId, FromVer) ->
    get_children(DocKey, ViewId, PageId, FromVer, undefined).


get_children(DocKey, ViewId, PageId, FromVer, undefined) ->
    case get_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_children(DocPid, DocKey, ViewId,
                                               PageId, FromVer)
    end;

get_children(DocKey, ViewId, PageId, FromVer, WatcherPid)
  when is_pid(WatcherPid) ->
    Item = {DocKey, WatcherPid},
    ets:insert(?WATCHER_KEY_TO_PID, Item),
    ets:insert(?WATCHER_PID_TO_KEY, Item),
    case get_document(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, DocPid} ->
            erod_document_process:get_children(DocPid, DocKey, ViewId,
                                               PageId, FromVer, WatcherPid)
    end.


notify_change(DocKey, Patch) ->
    try ets:lookup_element(?WATCHER_KEY_TO_PID, DocKey, 2) of
        Watchers -> notify_change_impl(Watchers, DocKey, Patch)
    catch error:badarg -> ok end.


perform(Action, Args, Ctx) ->
    perform_action(Action, Args, Ctx).


init([]) ->
    lager:info("Starting document registry...", []),
    process_flag(trap_exit, true),
    Factories = load_factories(),
    _ = ets:new(?WATCHER_KEY_TO_PID, [named_table, public, bag, {keypos, 1}]),
    _ = ets:new(?WATCHER_PID_TO_KEY, [named_table, public, bag, {keypos, 2}]),
    _ = ets:new(?DOCUMENT_KEY_TO_PID, [named_table, public, set, {keypos, 1}]),
    _ = ets:new(?DOCUMENT_PID_TO_KEY, [named_table, public, bag, {keypos, 2}]),
    {ok, #?St{factories = Factories}}.


handle_call({find_document, DocKey}, _From, State) ->
    case lookup_or_create(DocKey, State) of
        {ok, DocPid, NewState} -> {reply, {ok, DocPid}, NewState};
        {error, Reason, NewState} -> {reply, {error, Reason}, NewState}
    end;

handle_call(Request, {From, _Ref}, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


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
    lager:debug("Document ~p died, cleaning the registry...", [Pid]),
    unregister_interests_impl(Pid),
    unregister_document_impl(Pid, dead),
    {noreply, State};

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:info("Terminating document registry: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.



load_factories() ->
    {ok, AppName} = application:get_application(),
    FactorySpecs = application:get_env(AppName, document_factories, []),
    erod_maps:from_items([{T, {M, O}} || {T, M, O} <- FactorySpecs]).


lookup_or_create(DocKey, State) ->
    try ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        DocPid -> {ok, DocPid, State}
    catch
        error:badarg -> create_document(DocKey, State)
    end.


create_document({Type, _} = DocKey, #?St{factories = Factories} = State) ->
    case erod_maps:lookup(Type, Factories) of
        none -> {error, document_not_found, State};
        {value, {Module, Options}} ->
            case Module:start_document(DocKey, Options) of
                {ok, DocPid} -> {ok, DocPid, State};
                {error, Reason} -> {error, Reason, State}
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


perform_action(get_content, _Args, Ctx) ->
    erod_context:failed(not_implemented, Ctx),
    ok;

perform_action(get_children, _Args, Ctx) ->
    erod_context:failed(not_implemented, Ctx),
    ok;

perform_action(Action, Args, Ctx) ->
    erod_context:error("Registry do not know how to perform action ~p with "
                       "arguments ~p.", [Action, Args], Ctx),
    erod_context:failed(unknown_action, Ctx),
    ok.


