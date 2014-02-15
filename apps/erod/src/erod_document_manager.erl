-module(erod_document_manager).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([find_document/1]).
-export([register_document/2]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(St, ?MODULE).
-define(DOCUMENT_KEY_TO_PID, erod_document_key_to_pid).
-define(DOCUMENT_PID_TO_KEY, erod_document_pid_to_key).


-record(?St, {key_to_pid :: ets:tid(),
              pid_to_key :: ets:tid()}).


start_link() ->
    gen_server:start_link({local, ?DOCUMENT_MANAGER}, ?MODULE, [], []).


find_document(DocKey) ->
    try ets:lookup(?DOCUMENT_KEY_TO_PID, DocKey) of
        [] -> gen_server:call(?DOCUMENT_MANAGER, {find_document, DocKey});
        [{_, DocPid}] -> {ok, DocPid}
    catch
        % FIXME: Remove this defensive code.
        eror:badarg ->
            lager:error("Tried to find a document by its key but the ETS "
                        "table does not seem to exist yet"),
            {error, internal_error}
    end.


register_document(DocKey, DocPid) ->
    gen_server:call(?DOCUMENT_MANAGER, {register_document, DocKey, DocPid}).


init([]) ->
    lager:info("Starting document manager...", []),
    process_flag(trap_exit, true),
    K2P = ets:new(?DOCUMENT_KEY_TO_PID, [named_table, protected]),
    P2K = ets:new(?DOCUMENT_PID_TO_KEY, [bag, private, {keypos, 2}]),
    {ok, #?St{key_to_pid = K2P, pid_to_key = P2K}}.


handle_call({find_document, DocKey}, _From, State) ->
    case lookup_or_start_document(DocKey, State) of
        {ok, DocPid, NewState} -> {reply, {ok, DocPid}, NewState};
        {error, Reason, NewState} -> {reply, {error, Reason}, NewState}
    end;

handle_call({register_document, DocKey, DocPid}, _From, State) ->
    case add_document(DocKey, DocPid, State) of
        {ok, DocPid, NewState} -> {reply, {ok, DocPid}, NewState};
        {error, Reason, NewState} -> {reply, {error, Reason}, NewState}
    end;

handle_call(Request, From, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info({'DOWN', _, process, Pid, _Reason}, State) ->
    lager:debug("Document ~p died, removing it from the lookup table...", [Pid]),
    {ok, NewState} = remove_documents(Pid, State),
    {noreply, NewState};

handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:debug("Terminating user manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


lookup_or_start_document(DocKey, #?St{key_to_pid = K2P} = State) ->
    case ets:lookup(K2P, DocKey) of
        [{_, DocPid}] -> {ok, DocPid, State};
        [] -> start_document(DocKey, State)
    end.


start_document(DocKey, State) ->
    case erod_document_sup:start_child(DocKey) of
        {error, Reason} -> {error, Reason, State};
        {ok, DocPid} -> add_document(DocKey, DocPid, State)
    end.


add_document(DocKey, DocPid, #?St{pid_to_key = P2K, key_to_pid = K2P} = State) ->
    Item = {DocKey, DocPid},
    case ets:insert_new(K2P, Item) of
        false -> {error, already_registered, State};
        true ->
            erlang:monitor(process, DocPid),
            ets:insert(P2K, Item),
            {ok, DocPid, State}
    end.


remove_documents(DocPid, #?St{pid_to_key = P2K, key_to_pid = K2P} = State) ->
    case ets:lookup(P2K, DocPid) of
        [] -> {ok, State};
        Items ->
            _ = [ets:delete(K2P, K) || {K, _} <- Items],
            ets:delete(P2K, DocPid),
            {ok, State}
    end.
