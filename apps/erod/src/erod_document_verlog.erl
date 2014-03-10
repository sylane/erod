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
%%% @doc TODO: Document module erod_document_verlog.
%%% @end
%%% @private
%%% ==========================================================================

-module(erod_document_verlog).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/0,
         version/1,
         add_patch/2,
         add_patch/3,
         commit/1,
         get_patch/2]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(VerLog, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?VerLog, {identity :: pos_integer(),
                  version :: pos_integer(),
                  current :: [erod:patch_entry()] | [],
                  history :: erodlib:emap()}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type verlog() :: #?VerLog{}.

-export_type([verlog/0]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new empty version log.
%% @end
%% -----------------------------------------------------------------
-spec new() -> VerLog
    when VerLog :: verlog().
%% -----------------------------------------------------------------

new() ->
    Identity = crypto:rand_uniform(10000000,100000000),
    #?VerLog{identity = Identity, version = 1,
             current = [], history = erodlib_maps:new()}.


%% -----------------------------------------------------------------
%% @doc Gives the current version identifier of the history log.
%% @end
%% -----------------------------------------------------------------
-spec version(VerLog) -> Version
    when VerLog :: verlog(), Version :: erod:ersion().
%% -----------------------------------------------------------------

version(#?VerLog{identity = Identity, version = Version}) ->
    [Identity, Version].


%% -----------------------------------------------------------------
%% @doc Stages a patch or a patch entry to be part of the next version.
%% The version will be created when calling {@link commit/1}.
%% @end
%% -----------------------------------------------------------------
-spec add_patch(EntryOrPatch, VerLog) -> VerLog
    when EntryOrPatch :: erod:patch_entry() | erod:patch(), VerLog :: verlog().
%% -----------------------------------------------------------------

add_patch(Entry, #?VerLog{current = Current} = VerLog) when is_tuple(Entry) ->
    VerLog#?VerLog{current = [Entry |Current]};

add_patch(Patch, #?VerLog{current = Current} = VerLog) when is_list(Patch) ->
    VerLog#?VerLog{current = lists:reverse(Patch, Current)}.


%% -----------------------------------------------------------------
%% @doc Stages a patch to be part of the next version after adding
%% a prefi to all entries' path components.
%% The version will be created when calling {@link commit/1}.
%% @end
%% -----------------------------------------------------------------
-spec add_patch(Prefix, Patch, VerLog) -> VerLog
    when Prefix :: erod:patch_path_part(),
         Patch :: erod:patch(), VerLog :: verlog().
%% -----------------------------------------------------------------

add_patch(Prefix, Patch, #?VerLog{current = Current} = VerLog)
  when is_list(Patch), is_integer(Prefix) orelse is_binary(Prefix) ->
    Prefixed = [add_prefix(Prefix, Entry) || Entry <- Patch],
    VerLog#?VerLog{current = lists:reverse(Prefixed, Current)}.


%% -----------------------------------------------------------------
%% @doc Commits all the stages patches in a new version if there is some.
%% Returns if a new version has been generated.
%% @end
%% -----------------------------------------------------------------
-spec commit(VerLog) -> {HasChanged, VerLog}
    when VerLog :: verlog(), HasChanged :: boolean().
%% -----------------------------------------------------------------

commit(#?VerLog{current = []} = VerLog) -> {false, VerLog};

commit(#?VerLog{version = Version, current = Current, history = History} = VerLog) ->
    NewHistory = erodlib_maps:insert(Version, lists:reverse(Current), History),
    {true, VerLog#?VerLog{version = Version + 1,
                          current = [], history = NewHistory}}.


%% -----------------------------------------------------------------
%% @doc Gives the cumulated patches to get from specified version
%% to the last one. If the specified version is not in the history
%% or is undefined, the functino returns 'none'. If the specified version
%% is the last one the functino returns 'unchanged.
%% @end
%% -----------------------------------------------------------------
-spec get_patch(Version, VerLog) -> none | unchanged | {patch, Version, Patch}
    when Version :: erod:version(), VerLog :: verlog(), Patch :: erod:patch().
%% -----------------------------------------------------------------

get_patch(undefined, _VerLog) -> none;

get_patch([Identity, Ver], #?VerLog{identity = Identity, version = Ver}) ->
    unchanged;

get_patch([Identity, FromVer], #?VerLog{identity = Identity} = VerLog) ->
    #?VerLog{version = Version, history = History} = VerLog,
    case erodlib_maps:lookup_from(FromVer, History) of
        none -> none;
        {values, Patches} ->
            {patch, [Identity, Version], lists:flatten(Patches)}
    end;

get_patch(_FromVer, _VerLog) -> none.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

add_prefix(Prefix, {remove, Path}) ->
    {remove, [Prefix| Path]};

add_prefix(Prefix, {Op, Path, Value})
  when Op =:= add; Op =:= replace ->
    {Op, [Prefix| Path], Value};

add_prefix(Prefix, {Op, Path1, Path2})
  when Op =:= move; Op =:= copy ->
    {Op, [Prefix| Path1], [Prefix| Path2]}.
