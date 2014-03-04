-module(erod_document_process).

-export([get_content/3,
         get_content/4,
         get_children/5,
         get_children/6,
         add_watcher/3,
         del_watcher/3,
         notify_change/3,
         notify_state/3]).

-export([perform/4]).


-define(DEFAULT_TIMEOUT, 5000).


get_content(DocPid, DocKey, FromVer) ->
    call(DocPid, DocKey, {get_content, FromVer, undefined}).


get_content(DocPid, DocKey, FromVer, WatcherPid) when is_pid(WatcherPid) ->
    call(DocPid, DocKey, {get_content, FromVer, WatcherPid}).


get_children(DocPid, DocKey, ViewId, PageId, FromVer) ->
    call(DocPid, DocKey, {get_children, ViewId, PageId, FromVer, undefined}).


get_children(DocPid, DocKey, ViewId, PageId, FromVer, WatcherPid) ->
    call(DocPid, DocKey, {get_children, ViewId, PageId, FromVer, WatcherPid}).


add_watcher(_DocPid, _DocKey, _WatcherPid) ->
    ok.


del_watcher(_DocPid, _DocKey, _WatcherPid) ->
    ok.


notify_change(_WatcherPid, _DocKey, _Patch) ->
    ok.


notify_state(_WatcherPid, _DocKey, _StateName) ->
    ok.


perform(DocPid, Action, Args, Ctx) ->
    DocPid ! {'$doc_perform', Action, Args, Ctx},
    ok.



call(Pid, Label, Request) ->
    call(Pid, Label, Request, ?DEFAULT_TIMEOUT).


call(Pid, DocKey, Request, Timeout)
  when is_pid(Pid), Timeout =:= infinity;
       is_pid(Pid), is_integer(Timeout), Timeout >= 0 ->
    MonRef = erlang:monitor(process, Pid),
    Msg = {'$doc_call', DocKey, {self(), MonRef}, Request},
    catch erlang:send(Pid, Msg, [noconnect]),
    receive
        {MonRef, Reply} ->
            erlang:demonitor(MonRef, [flush]),
            Reply;
        {'DOWN', MonRef, _, _, noconnection} ->
            exit({nodedown, node(Pid)});
        {'DOWN', MonRef, _, _, Reason} ->
            exit(Reason)
    after Timeout ->
            erlang:demonitor(MonRef, [flush]),
            exit(timeout)
    end.
