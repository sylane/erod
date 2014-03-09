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
%%% @doc TODO: Document module erod_docuemnt_page.
%%% Requirments:
%%%    - Fast Item lookup by key
%%%    - Ordered by given comparison function
%%%    - Relatively fast insert/delete/update by key
%%%    - Optional reordering on update.
%%%    - Keep track of the version
%%%    - Keep a log of changes in a JSON patch-like format.
%%% @end
%%% ==========================================================================

-module(erod_document_page).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/0,
         from_orddict/3,
         size/1,
         smallest/1,
         insert/5,
         delete/4,
         update_inplace/6,
         update_order/6,
         commit/1,
         get_content/4]).

%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(Page, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?Page, {indice :: indice(),
                first :: term() | undefined,
                last :: term() | undefined,
                verlog :: verlog()}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type page() :: #?Page{}.

-type verlog() :: erod_document_verlog:verlog().
-type indice() :: erodlib:indice().
-type emap() :: erodlib:emap().
-type compare_fun() :: erodlib:compare_fun().
-type map_fun() :: erodlib:map_fun().

-export_type([page/0]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new empty document page.
%% @end
%% -----------------------------------------------------------------
-spec new() -> Page
    when Page :: page().
%% -----------------------------------------------------------------

new() ->
    #?Page{indice = erodlib_indices:new(),
           first = undefined, last = undefined,
           verlog = erod_document_verlog:new()}.


%% -----------------------------------------------------------------
%% @doc Creates a new document page forma an ordered list of items.
%% @end
%% -----------------------------------------------------------------
-spec from_orddict(Items, CompareFun, Map) -> Page
    when Items :: [{Key, Value}] | [], Key :: term(), Value :: term(),
         CompareFun :: compare_fun(), Map :: emap(), Page :: page().
%% -----------------------------------------------------------------

from_orddict([], _CompareFun, _Map) -> new();

from_orddict(ValOrdDict, _CompFun, Map) ->
    Indice = erodlib_indices:from_orddict(ValOrdDict),
    {_, First} = hd(ValOrdDict),
    Last = erodlib_indices:largest_value(Map, Indice),
    #?Page{indice = Indice,
           first = First, last = Last,
           verlog = erod_document_verlog:new()}.


%% -----------------------------------------------------------------
%% @doc Gives the numer of elements in the page.
%% @end
%% -----------------------------------------------------------------
-spec size(Page) -> Size
    when Page :: page(), Size :: non_neg_integer().
%% -----------------------------------------------------------------

size(#?Page{indice = Indice}) ->
    erodlib_indices:size(Indice).


%% -----------------------------------------------------------------
%% @doc Gives the smallest value of the page or undefined if the page is empty.
%% @end
%% -----------------------------------------------------------------
-spec smallest(Page) -> Value
    when Page :: page(), Value :: term() | undefined.
%% -----------------------------------------------------------------

smallest(#?Page{first = Val}) ->
    Val.


%% -----------------------------------------------------------------
%% @doc Inserts a key and value pair in the page with specified comparison
%% function to find out the pair position in the page.
%%
%% If the value in the specified map for the given key is different
%% from the specified value the outcome of the function is undetermined.
%%
%% Assume the key is not present in the page, crash otherwise.
%% @end
%% -----------------------------------------------------------------
-spec insert(Key, Value, CompFun, Map, Page) -> Page
    when Key :: term(), Value :: term(), CompFun :: compare_fun(),
         Map :: emap(), Page :: page().
%% -----------------------------------------------------------------

insert(Key, Val, CompFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    {Idx, Indice2} = erodlib_indices:insert(Key, Val, CompFun, Map, Indice),
    Size = erodlib_indices:size(Indice2),
    Page2 = Page#?Page{indice = Indice2},
    Page3 = insert_fix_barrier(Val, Idx, Size, Page2),
    add_patch({add, [Idx], Val}, Page3).


%% -----------------------------------------------------------------
%% @doc Deletes a key and value pair from the page with specified comparison
%% function to find out the pair position in the page.
%%
%% Assume the key is present in the page, crash otherwise.
%% @end
%% -----------------------------------------------------------------
-spec delete(Key, CompFun, Map, Page) -> Page
    when Key :: term(), CompFun :: compare_fun(),
         Map :: emap(), Page :: page().
%% -----------------------------------------------------------------

delete(Key, CompFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    Size = erodlib_indices:size(Indice),
    {Idx, Indice2} = erodlib_indices:delete(Key, CompFun, Map, Indice),
    Page2 = Page#?Page{indice = Indice2},
    Page3 = delete_fix_barrier(Idx, Size, Map, Page2),
    add_patch({remove, [Idx]}, Page3).


%% -----------------------------------------------------------------
%% @doc Updates a page's value that do not need any reordering.
%%
%% The specified patch MUST contains the instructions that applied
%% to the current page value would give the new value, otherwise
%% the version journal would be broken.
%% @end
%% -----------------------------------------------------------------
-spec update_inplace(Key, Value, Patch, CompFun, Map, Page) -> Page
    when Key :: term(), Value :: term(), Patch :: erod:patch(),
         CompFun :: compare_fun(), Map :: emap(), Page :: page().
%% -----------------------------------------------------------------

update_inplace(Key, Val, Patch, CompFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    Pos = erodlib_indices:position(Key, Val, CompFun, Map, Indice),
    Size = erodlib_indices:size(Indice),
    Page2 = update_fix_barrier(Val, Pos, Pos, Size, Map, Page),
    add_patch(Pos, Patch, Page2).


%% -----------------------------------------------------------------
%% @doc Updates a page's value that could need reordering.
%%
%% If the page cannot know if the value is still part of itself,
%% it will delete the value and let the caller resolve where it
%% should be inserted back.
%%
%% The specified patch MUST contains the instructions that applied
%% to the current page value would give the new value, otherwise
%% the version journal would be broken.
%% @end
%% -----------------------------------------------------------------
-spec update_order(Key, Value, Patch, CompFun, Map, Page)
    -> {ok, Page} | {deleted, Page}
    when Key :: term(), Value :: term(), Patch :: erod:patch(),
         CompFun :: compare_fun(), Map :: emap(), Page :: page().
%% -----------------------------------------------------------------

update_order(Key, Val, Patch, CompFun, Map, Page) ->
    #?Page{indice = Idx} = Page,
    case is_local(Val, CompFun, Page) of
        false ->
            Size = erodlib_indices:size(Idx),
            {I, Idx2} = erodlib_indices:delete(Key, Val, CompFun, Map, Idx),
            Page2 = Page#?Page{indice = Idx2},
            Page3 = delete_fix_barrier(I, Size, Map, Page2),
            {deleted, add_patch([{remove, [I]}], Page3)};
        true ->
            {I1, Idx2} = erodlib_indices:delete(Key, Val, CompFun, Map, Idx),
            {I2, Idx3} = erodlib_indices:insert(Key, Val, CompFun, Map, Idx2),
            Size = erodlib_indices:size(Idx3),
            Page2 = Page#?Page{indice = Idx3},
            Page3 = update_fix_barrier(Val, I1, I2, Size, Map, Page2),
            {ok, add_patch({move, [I1], [I2]}, add_patch(I1, Patch, Page3))}
    end.


%% -----------------------------------------------------------------
%% @doc Commits all the changes since the last commit into a new page version.
%% Returns if the page changed or not since the last commit.
%% @end
%% -----------------------------------------------------------------
-spec commit(Page) -> {HasChanged, Page}
    when Page :: page(), HasChanged :: boolean().
%% -----------------------------------------------------------------

commit(#?Page{verlog = VerLog} = Page) ->
    {Changed, NewVerLog} = erod_document_verlog:commit(VerLog),
    {Changed, Page#?Page{verlog = NewVerLog}}.


%% -----------------------------------------------------------------
%% @doc Gives the content of the page from a given version.
%%
%% If the specified version undefined or not found in the history
%% the full content of the page is returned as a list of items,
%% alongside the new version, the page size and the total size of the document.
%%
%% If the specified version is found in the history, only a cumulative patch
%% to get to the last version from the specified version is returned,
%% alongside the new version, the page size and the total size of the document.
%%
%% If the page did not change since the specified version
%% 'unchanged' is returned.
%%
%% The specified function is applied on all the page items
%% to generate the result set if the complete entity is returned.
%% @end
%% -----------------------------------------------------------------
-spec get_content(Version, MapFun, Map, Page)
    -> unchanged
     | {entity, Version, PageSize, TotalSize, Data}
     | {patch, version, PageSize, TotalSize, Patch}
    when Version :: erod:version(), MapFun :: map_fun(), Map :: emap(),
         Page :: page(), PageSize :: non_neg_integer(),
         TotalSize :: non_neg_integer(), Data :: [term()] | [],
         Patch :: erod:patch().
%% -----------------------------------------------------------------

get_content(FromVer, MapFun, Map, #?Page{indice = Indice, verlog = VerLog}) ->
    PageSize = erodlib_indices:size(Indice),
    TotalSize = erodlib_maps:size(Map),
    case erod_document_verlog:get_patch(FromVer, VerLog) of
        unchanged -> unchanged;
        none ->
            Version = erod_document_verlog:version(VerLog),
            PageData = erodlib_indices:map(MapFun, Map, Indice),
            {entity, Version, PageSize, TotalSize, PageData};
        {patch, Version, Patch} ->
            {patch, Version, PageSize, TotalSize, Patch}
    end.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

add_patch(Patch, #?Page{verlog = VerLog} = Page) ->
    Page#?Page{verlog = erod_document_verlog:add_patch(Patch, VerLog)}.


add_patch(Prefix, Patch, #?Page{verlog = VerLog} = Page) ->
    Page#?Page{verlog = erod_document_verlog:add_patch(Prefix, Patch, VerLog)}.


is_local(Val, CompFun, #?Page{first = First, last = Last}) ->
    (not CompFun(Val, First)) andalso CompFun(Val, Last).


%%% Size BEFORE deletion
delete_fix_barrier(1, 1, _Map, Page) ->
    % The last value has been deleted
    Page#?Page{first = undefined, last = undefined};

delete_fix_barrier(1, 2, _Map, Page) ->
    % The first value has been deleted and there is only one left
    Page#?Page{first = Page#?Page.last};

delete_fix_barrier(2, 2, _Map, Page) ->
    % The last value has been deleted and there is only one left
    Page#?Page{last = Page#?Page.first};

delete_fix_barrier(1, _Size, Map, Page) ->
    % The first value has been delete
    NewFirst = erodlib_indices:smallest_value(Map, Page#?Page.indice),
    Page#?Page{first = NewFirst};

delete_fix_barrier(From, From, Map, Page) ->
    % The last value has been delete
    NewLast = erodlib_indices:largest_value(Map, Page#?Page.indice),
    Page#?Page{last = NewLast};

delete_fix_barrier(_From, _Sie, _Map, Page) ->
    % The change is not relevent for the barrier
    Page.


%%% Size AFTER insertion
insert_fix_barrier(Val, 1, 1, Page) ->
    % The first value to be inserted
    Page#?Page{first = Val, last = Val};

insert_fix_barrier(Val, 1, _, Page) ->
    % The value has been inserted at the first position
    Page#?Page{first = Val};

insert_fix_barrier(Val, Idx, Idx, Page) ->
    % The value has been inserted at the last position
    Page#?Page{last = Val};

insert_fix_barrier(_Value, _Idx, _Size, Page) ->
    % The value position is not relevent for the barrier
    Page.


update_fix_barrier(Val, 1, 1, 1, _Map, Page) ->
    % The only value changed
    Page#?Page{first = Val, last = Val};

update_fix_barrier(Val, 1, 1, _Size, _Map, Page) ->
    % The first value changed
    Page#?Page{first = Val};

update_fix_barrier(Val, Idx, Idx, Idx, _Map, Page) ->
    % The last value changed
    Page#?Page{last = Val};

update_fix_barrier(_Value, Idx, Idx, _Size, _Map, Page) ->
    % The value index did not change and it is neither first or last
    Page;

update_fix_barrier(Val, 1, To, To, Map, Page) ->
    % The value moved from the first position to the last position
    NewFirst = erodlib_indices:smallest_value(Map, Page#?Page.indice),
    Page#?Page{first = NewFirst, last = Val};

update_fix_barrier(Val, To, 1, To, Map, Page) ->
    % The value moved from the last position to the first position
    NewLast = erodlib_indices:largest_value(Map, Page#?Page.indice),
    Page#?Page{first = Val, last = NewLast};

update_fix_barrier(Val, _From, To, To, _Map, Page) ->
    % The value became to the last one
    Page#?Page{last = Val};

update_fix_barrier(_Value, From, _To, From, Map, Page) ->
    % The value is not the last one anymore
    NewLast = erodlib_indices:largest_value(Map, Page#?Page.indice),
    Page#?Page{last = NewLast};

update_fix_barrier(Val, _From, 1, _Size, _Map, Page) ->
    % The value became the first one
    Page#?Page{first = Val};

update_fix_barrier(_Value, 1, _To, _Size, Map, Page) ->
    % The value is not the first one anymore
    NewFirst = erodlib_indices:smallest_value(Map, Page#?Page.indice),
    Page#?Page{first = NewFirst};

update_fix_barrier(_Value, _From, _To, _Size, _Map, Page) ->
    % Nothing relevent for the barrier
    Page.
