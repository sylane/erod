-module(erod_document_manager).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([add_locator/3,
         register_document/2,
         unregister_document/1,
         unregister_document/2,
         find_document/1,
         retrieve_interests/1,
         register_interest/2,
         unregister_interest/1,
         unregister_interest/2,
         notify_change/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PROCESS, ?MODULE).
-define(St, ?MODULE).
-define(INTEREST_KEY_TO_PID, erod_interest_key_to_pid).
-define(INTEREST_PID_TO_KEY, erod_interest_pid_to_key).
-define(DOCUMENT_KEY_TO_PID, erod_document_key_to_pid).
-define(DOCUMENT_PID_TO_KEY, erod_document_pid_to_key).


-record(?St, {}).


start_link() ->
    gen_server:start_link({local, ?PROCESS}, ?MODULE, [], []).


add_locator(_Type, _Module, _Options) ->
    %TODO
    ok.


register_document(DocKey, DocPid) ->
    gen_server:cast(?PROCESS, {link_document, DocPid}),
    Item = {DocKey, DocPid},
    ets:insert(?DOCUMENT_PID_TO_KEY, Item),
    ets:insert(?DOCUMENT_KEY_TO_PID, Item),
    ok.


unregister_document(DocKey) ->
    unregister_document_impl(DocKey, unregistered).


unregister_document(DocKeys, DocPid) when is_list(DocKeys) ->
    unregister_documents_impl(DocKeys, DocPid, unregistered);

unregister_document(DocKey, DocPid) ->
    unregister_document_impl(DocKey, DocPid, unregistered).


find_document(DocKey) ->
    %TODO: What to do with document not found ?
    case ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        [] -> gen_server:call(?PROCESS, {find_document, DocKey});
        [DocPid] -> {ok, DocPid}
    end.


retrieve_interests(DocKey) ->
    ets:lookup_element(?INTEREST_KEY_TO_PID, DocKey, 2).


register_interest(DocKey, WatcherPid) ->
    gen_server:cast(?PROCESS, {link_document, WatcherPid}),
    Item = {DocKey, WatcherPid},
    ets:insert(?INTEREST_KEY_TO_PID, Item),
    ets:insert(?INTEREST_PID_TO_KEY, Item),
    case ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        [] -> ok;
        [DocPid] ->
            erod_document:register_interest(DocPid, DocKey, WatcherPid),
            ok
    end.


unregister_interest(WatcherPid) ->
    % No registration must be done for this watcher in parallele
    gen_server:cast(?PROCESS, {unlink_document, WatcherPid}),
    unregister_interests_impl(WatcherPid).


unregister_interest(DocKeys, WatcherPid) when is_list(DocKeys) ->
    unregister_interests_impl(DocKeys, WatcherPid);

unregister_interest(DocKey, WatcherPid) ->
    unregister_interest_impl(DocKey, WatcherPid).


notify_change(DocKey, Patch) ->
    Watchers = ets:lookup_element(?INTEREST_KEY_TO_PID, DocKey, 2),
    notify_change_impl(Watchers, DocKey, Patch).


init([]) ->
    lager:info("Starting document manager...", []),
    process_flag(trap_exit, true),
    _ = ets:new(?INTEREST_KEY_TO_PID, [named_table, public, bag, {keypos, 1}]),
    _ = ets:new(?INTEREST_PID_TO_KEY, [named_table, public, bag, {keypos, 2}]),
    _ = ets:new(?DOCUMENT_KEY_TO_PID, [named_table, public, set, {keypos, 1}]),
    _ = ets:new(?DOCUMENT_PID_TO_KEY, [named_table, public, bag, {keypos, 2}]),
    {ok, #?St{}}.


handle_call({find_document, _DocKey}, _From, State) ->
    {reply, {error, not_found}, State};

handle_call(Request, From, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast({link_document, Pid}, State) ->
    catch link(Pid),
    {reply, ok, State};

handle_cast({unlink_document, Pid}, State) ->
    catch unlink(Pid),
    {reply, ok, State};

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
    lager:debug("Terminating user manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


unregister_document_impl(DocPid, Reason) when is_pid(DocPid) ->
    DocKeys = ets:lookup_element(?DOCUMENT_PID_TO_KEY, DocPid, 1),
    unregister_documents_impl(DocKeys, DocPid, Reason);

unregister_document_impl(DocKey, Reason) ->
    case ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        [DocPid] -> unregister_document_impl(DocKey, DocPid, Reason);
        [] -> ok
    end.


unregister_documents_impl([], _DocPid, _Reason) -> ok;

unregister_documents_impl([DocKey |DocKeys], DocPid, Reason) ->
    unregister_document_impl(DocKey, DocPid, Reason),
    unregister_documents_impl(DocKeys, DocPid, Reason).


unregister_document_impl(DocKey, DocPid, Reason) ->
    ets:delete_object(?DOCUMENT_PID_TO_KEY, {DocKey, DocPid}),
    ets:delete(?DOCUMENT_KEY_TO_PID, DocKey),
    Watchers = ets:lookup_element(?INTEREST_KEY_TO_PID, DocKey, 2),
    notify_state_impl(Watchers, DocKey, Reason),
    ok.


unregister_interests_impl(WatcherPid) ->
    Keys = ets:lookup_element(?INTEREST_PID_TO_KEY, WatcherPid, 1),
    unregister_interest(Keys, WatcherPid).


unregister_interests_impl([], _WatcherPid) -> ok;

unregister_interests_impl([DocKey |DocKeys], WatcherPid) ->
    unregister_interest_impl(DocKey, WatcherPid),
    unregister_interests_impl(DocKeys, WatcherPid).


unregister_interest_impl(DocKey, WatcherPid) ->
    case ets:lookup_element(?DOCUMENT_KEY_TO_PID, DocKey, 2) of
        [] -> ok;
        [DocPid] ->
            erod_document:unregister_interest(DocPid, DocKey, WatcherPid)
    end,
    Item = {DocKey, WatcherPid},
    ets:delete_object(?INTEREST_KEY_TO_PID, Item),
    ets:delete_object(?INTEREST_PID_TO_KEY, Item),
    ok.


notify_change_impl([], _DocKey, _Patch) -> ok;

notify_change_impl([WatcherPid |Watchers], DocKey, Patch) ->
    erod_document:notify_change(WatcherPid, DocKey, Patch),
    notify_change_impl(Watchers, DocKey, Patch).


notify_state_impl([], _DocKey, _State) -> ok;

notify_state_impl([WatcherPid |Watchers], DocKey, State) ->
    erod_document:notify_state(WatcherPid, DocKey, State),
    notify_state_impl(Watchers, DocKey, State).
