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
%%% @doc TODO: Document module erod_document_view.
%%% @end
%%% @private
%%% ==========================================================================

-module(erod_document_view).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/2,
         from_items/4,
         insert/4,
         delete/3,
         update_inplace/5,
         update_order/5,
         commit/1,
         get_content/5]).

%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(View, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?View, {page_size :: pos_integer(),
                compare_fun :: compare_fun(),
                changed_pages :: eset(),
                pages :: erodlib:emap()}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

%%% Imported types
-type compare_fun() :: erodlib:compare_fun().
-type map_fun() :: erodlib:map_fun().
-type eset() :: erodlib:eset().
-type emap() :: erodlib:emap().

-type view() :: #?View{}.
-export_type([view/0]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates an empty view with specified page size and comparison function.
%% @end
%% -----------------------------------------------------------------
-spec new(PageSize, CompareFun) -> View
    when PageSize :: pos_integer(), CompareFun :: compare_fun(), View :: view().
%% -----------------------------------------------------------------

new(PageSize, CompareFun) ->
    #?View{page_size = PageSize,
           compare_fun = CompareFun,
           changed_pages = erodlib_sets:new(),
           pages = erodlib_maps:new()}.


%% -----------------------------------------------------------------
%% @doc Creates a view with specified page size and comparison function
%% and an unordered list of items.
%% All items must be defined in specified map, the behavior is
%% undefined otherwise.
%% @end
%% -----------------------------------------------------------------
-spec from_items(Items, PageSize, CompareFun, Map) -> View
    when Items :: [Item] | [], Item :: {Key, Val}, Key :: term(), Val :: term(),
         PageSize :: pos_integer(), CompareFun :: compare_fun(),
         Map :: emap(), View :: view().
%% -----------------------------------------------------------------

from_items([], PageSize, CompareFun, _Map) ->
    new(PageSize, CompareFun);

from_items(Items, PageSize, CompareFun, Map) ->
    Fun = fun({_, Va}, {_, Vb}) -> CompareFun(Va, Vb) end,
    Sorted = lists:sort(Fun, Items),
    Pages = create_pages(Sorted, PageSize, CompareFun, Map),
    #?View{page_size = PageSize,
           compare_fun = CompareFun,
           changed_pages = erodlib_sets:new(),
           pages = Pages}.


%% -----------------------------------------------------------------
%% @doc Inserts a key/value in the view.
%%
%% The pair must be defined in the specified map,
%% the behavior is undefined otherwise.
%%
%% Assumes the view doesn't contain the key, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec insert(Key, Value, Map, View) -> View
%%     when Key :: term(), Value :: term(), Map :: emap(), View :: view().
%% -----------------------------------------------------------------

insert(_Key, _Value, _Map, View) ->
    View.


%% -----------------------------------------------------------------
%% @doc Deletes a key from the view.
%%
%% The key must still exists in the specified map,
%% the behavior is undefined otherwise.
%%
%% Assumes the view contains the key, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec delete(Key, Map, View) -> View
%%     when Key :: term(), Map :: emap(), View :: view().
%% -----------------------------------------------------------------

delete(_Key, _Map, View) ->
    View.


%% -----------------------------------------------------------------
%% @doc Updates the view's value with specified key without reordering.
%%
%% The specified patch MUST contains the instructions that applied
%% to the current view value would give the new value, otherwise
%% the version journal would be broken.
%%
%% The specified map must contains the changed value for the given key,
%% the behavior is not defined otherwise.
%%
%% Assumes the view contains the key, craches otherwise.
%% @end
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec update_inplace(Key, Value, Patch, Map, View) -> View
%%     when Key :: term(), Value :: term(), Patch :: erod:patch(),
%%          Map :: emap(), View :: view().
%% -----------------------------------------------------------------

update_inplace(_Key, _Value, _Patch, _Map, View) ->
    View.


%% -----------------------------------------------------------------
%% @doc Updates the view's value with specified key that could need reordering.
%%
%% The specified map must contains the changed value for the given key,
%% the behavior is not defined otherwise.
%%
%% The specified patch MUST contains the instructions that applied
%% to the current page value would give the new value, otherwise
%% the version journal would be broken.
%% @end
%% -----------------------------------------------------------------
%% TODO: Uncomment when implemented.
%% -spec update_order(Key, Value, Patch, Map, View) -> View
%%     when Key :: term(), Value :: term(), Patch :: erod:patch(),
%%          Map :: emap(), View :: view().
%% -----------------------------------------------------------------

update_order(_Key, _Value, _Patch, _Map, View) ->
    View.


%% -----------------------------------------------------------------
%% @doc Commit all the changes into new versions.
%% Returns if any view's page version has changed.
%% @end
%% -----------------------------------------------------------------
-spec commit(View) -> {HasChanged, View}
    when View :: view(), HasChanged :: boolean().
%% -----------------------------------------------------------------

commit(#?View{changed_pages = ChangedPages, pages = Pages} = View) ->
    {Changed, Pages2} = commit_pages(erodlib_sets:values(ChangedPages), Pages),
    {Changed, View#?View{changed_pages = erodlib_sets:new(), pages = Pages2}}.


%% -----------------------------------------------------------------
%% @doc Gives the content of a page of the view, if possile returns
%% a patch from a given version identifier.
%%
%% If the page did not change since the specified version
%% 'unchanged' is returned.
%%
%% @see erod_document_page:get_content/4
%% @end
%% -----------------------------------------------------------------
-spec get_content(PageId, Version, MapFun, Map, View)
    -> unchanged
     | {error, Reason}
     | {entity, Version, PageSize, TotalSize, Data}
     | {patch, Version, PageSize, TotalSize, Patch}
    when PageId :: erod:page_id(), Version :: erod:version(),
         MapFun :: map_fun(), Map :: emap(),
         Reason :: page_not_found | term(),
         View :: view(), PageSize :: non_neg_integer(),
         TotalSize :: non_neg_integer(), Data :: [term()] | [],
         Patch :: erod:patch().
%% -----------------------------------------------------------------

get_content(PageId, FromVer, Fun, Map, #?View{pages = Pages}) ->
    case erodlib_maps:lookup(PageId, Pages) of
        none -> {error, page_not_found};
        {value, Page} ->
            erod_document_page:get_content(FromVer, Fun, Map, Page)
    end.


%%% ==========================================================================
%%% Internal Funtions
%%% ==========================================================================

commit_pages(Changes, Pages) ->
    commit_pages(Changes, Pages, false).


commit_pages([], Pages, Changed) ->
    {Changed, Pages};

commit_pages([Index |Rem], Pages, Changed) ->
    Page = erodlib_maps:value(Index, Pages),
    {PageChanged, NewPage} = erod_document_page:commit(Page),
    NewPages = erodlib_maps:update(Index, NewPage, Pages),
    commit_pages(Rem, NewPages, Changed or PageChanged).


create_pages(OrdValues, PageSize, CompareFun, Map) ->
    create_pages(OrdValues, PageSize, CompareFun, Map, 1, []).


create_pages([], _Size, _CompareFun, _Map, _Idx, Acc) ->
    erodlib_maps:from_orddict(lists:reverse(Acc));

create_pages(Values, Size, CompareFun, Map, Idx, Acc) ->
    {PageData, NewValues} = take_first_nth(Size, Values),
    Page = erod_document_page:from_orddict(PageData, CompareFun, Map),
    create_pages(NewValues, Size, CompareFun, Map, Idx + 1, [{Idx, Page} |Acc]).


take_first_nth(Size, List) ->
    take_first_nth(Size, List, []).


take_first_nth(0, List, Acc) ->
    {lists:reverse(Acc), List};

take_first_nth(_Size, [], Acc) ->
    {lists:reverse(Acc), []};

take_first_nth(Size, [Value |Rem], Acc) ->
    take_first_nth(Size - 1, Rem, [Value |Acc]).
