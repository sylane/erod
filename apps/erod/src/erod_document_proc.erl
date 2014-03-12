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
%%% @doc TODO: Document module erod_document_proc.
%%% @end
%%% ==========================================================================

-module(erod_document_proc).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([get_content/3,
         get_content/4,
         get_children/5,
         get_children/6]).

%%% Internal API functions
-export([add_watcher/3,
         del_watcher/3,
         notify_change/3,
         notify_state/3,
         perform/4]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).
-define(DEFAULT_TIMEOUT, 5000).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Gives the content of a document contained by specified process.
%% @see erod_document:get_content/2
%% @end
%% -----------------------------------------------------------------
-spec get_content(DocPid, DocKey, FromVer) -> {ok, Content} | unchanged
    when DocPid :: pid(), DocKey :: erod:key(),
         FromVer :: erod:version(), Content :: erod:content().
%% -----------------------------------------------------------------

get_content(DocPid, DocKey, FromVer) ->
    call(DocPid, DocKey, {get_content, FromVer, undefined}).


%% -----------------------------------------------------------------
%% @doc Gives the content of a document contained by specified process
%% and add a process as a document watcher for it if defined.
%% @see erod_document:get_content/2
%% @see erod_document:add_watcher/2
%% @end
%% -----------------------------------------------------------------
-spec get_content(DocPid, DocKey, FromVer, Watcher)
    -> unchanged | {ok, Content} | {error, Reason}
    when DocPid :: pid(), DocKey :: erod:key(), Watcher :: pid() | undefined,
         FromVer :: erod:version(), Content :: erod:content(),
         Reason :: document_not_found | term().
%% -----------------------------------------------------------------

get_content(DocPid, DocKey, FromVer, Watcher) ->
    call(DocPid, DocKey, {get_content, FromVer, Watcher}).


%% -----------------------------------------------------------------
%% @doc Gives a page of children from a document contained by specified process.
%% @see erod_document:get_children/4
%% @end
%% -----------------------------------------------------------------
-spec get_children(DocPid, DocKey, ViewId, PageId, FromVer)
    -> unchanged | {ok, Page} | {error, Reason}
    when DocPid :: pid(), DocKey :: erod:key(),
         ViewId :: erod:view_id(), PageId :: erod:page_id(),
         FromVer :: erod:version(), Page :: erod:page(),
         Reason :: document_not_found | view_not_found | page_not_found | term().
%% -----------------------------------------------------------------

get_children(DocPid, DocKey, ViewId, PageId, FromVer) ->
    call(DocPid, DocKey, {get_children, ViewId, PageId, FromVer, undefined}).


%% -----------------------------------------------------------------
%% @doc Gives a page of children from a document contained by specified process
%% and add a process as a document watcher for it if defined.
%% @see erod_document:get_children/4
%% @see erod_document:add_watcher/2
%% @end
%% -----------------------------------------------------------------
-spec get_children(DocPid, DocKey, ViewId, PageId, FromVer, Watcher)
    -> unchanged | {ok, Page} | {error, Reason}
    when DocPid :: pid(), DocKey :: erod:key(), ViewId :: erod:view_id(),
         PageId :: erod:page_id(), FromVer :: erod:version(),
         Watcher :: pid() | undefined, Page :: erod:page(),
         Reason :: document_not_found | view_not_found | page_not_found | term().
%% -----------------------------------------------------------------

get_children(DocPid, DocKey, ViewId, PageId, FromVer, Watcher) ->
    call(DocPid, DocKey, {get_children, ViewId, PageId, FromVer, Watcher}).


%%% ==========================================================================
%%% Inernal API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Adds a process to a document watcher list.
%% @end
%% @private
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec add_watcher(DocPid, DocKey, Watcher) -> ok | {error, Reason}
%%     when DocPid :: pid(), DocKey :: erod:key(),
%%          Watcher :: pid(), Reason :: document_not_found | term().
%% -----------------------------------------------------------------

add_watcher(_DocPid, _DocKey, _Watcher) ->
    ok.


%% -----------------------------------------------------------------
%% @doc Removes a process from a document watcher list.
%% @end
%% @private
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec del_watcher(DocPid, DocKey, Watcher) -> ok | {error, Reason}
%%     when DocPid :: pid(), DocKey :: erod:key(),
%%          Watcher :: pid(), Reason :: document_not_found | term().
%% -----------------------------------------------------------------

del_watcher(_DocPid, _DocKey, _Watcher) ->
    ok.


%% -----------------------------------------------------------------
%% @doc Notifies a document of a content change from a document it watches.
%% @end
%% @private
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec notify_change(WatcherPid, DocKey, Patch) -> ok | {error, Reason}
%%     when WatcherPid :: pid(), DocKey :: erod:key(),
%%          Patch :: erod:patch(), Reason :: document_not_found | term().
%% -----------------------------------------------------------------

notify_change(_WatcherPid, _DocKey, _Patch) ->
    ok.


%% -----------------------------------------------------------------
%% @doc Notifies a document of a state change from a document it watches.
%% @end
%% @private
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec notify_state(WatcherPid, DocKey, Patch) -> ok | {error, Reason}
%%     when WatcherPid :: pid(), DocKey :: erod:key(),
%%          Patch :: erod:patch(), Reason :: document_not_found | term().
%% -----------------------------------------------------------------

notify_state(_WatcherPid, _DocKey, _StateName) ->
    ok.

%% -----------------------------------------------------------------
%% @doc Performs an action for a document of specified process.
%% @end
%% @private
%% -----------------------------------------------------------------
-spec perform(DocPid, Action, Args, Context) -> ok
    when DocPid :: pid(), Action :: erod:action_id(),
         Args :: erod:action_args(), Context :: erod:context().
%% -----------------------------------------------------------------

perform(DocPid, Action, Args, Ctx) ->
    DocPid ! {'$doc_perform', Action, Args, Ctx},
    ok.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

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
