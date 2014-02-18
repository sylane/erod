%%% Requirments:
%%%    - Fast Item lookup by key
%%%    - Ordered by given comparison function
%%%    - Relatively fast insert/delete/update by key
%%%    - Optional reordering on update.
%%%    - Keep track of the version
%%%    - Keep a log of changes in a JSON patch-like format.

-module(erod_document_page).

-export([new/1,
         from_ordered/2,
         to_list/1,
         lookup/2,
         update/4,
         delete/2,
         insert/3]).

-define(Page, ?MODULE).
-record(?Page, {compare,
                items,
                indice,
                first,
                last,
                ver,
                log}).


new(CompareFun) ->
    #?Page{compare = CompareFun,
           items = erod_maps:new(),
           indice = erod_indices:new(),
           first = undefined, last = undefined,
           ver = 0, log = []}.


from_ordered(CompareFun, []) -> new(CompareFun);

from_ordered(CompareFun, ValOrdDict) ->
    KeyOrdDict = lists:keysort(1, ValOrdDict),
    Items = erod_maps:from_orddict(KeyOrdDict),
    Indice = erod_indices:from_ordered(ValOrdDict),
    First = hd(ValOrdDict),
    LastKey = erod_indice:largest(Indice),
    Last = {LastKey, erod_maps:value(LastKey)},
    #?Page{compare = CompareFun,
           items = Items, indice = Indice,
           first = First, last = Last,
           ver = 0, log = []}.


to_list(#?Page{items = Items, indice = Indice}) ->
    erod_indices:to_list(Items, Indice).


lookup(Key, #?Page{items = Items}) ->
    erod_maps:lookup(Key, Items).


%%% Not changing the order
update(Key, Value, Patch, Page) ->
    #?Page{compare = CompareFun, items = Items, indice = Indice} = Page,
    Idx = erod_indices:index(Key, Value, CompareFun, Items, Indice),
    NewItems = erod_maps:update(Key, Value, Items),
    NewPage = Page#?Page{items = NewItems},
    add_patch(Idx, Patch, update_fix_barrier(Key, Value, NewPage)).


delete(Key, Page) ->
    #?Page{compare = CompareFun, items = Items, indice = Indice} = Page,
    {Idx, NewIndice} = erod_indices:delete(Key, CompareFun, Items, Indice),
    NewItems = erod_maps:delete(Key, Items),
    NewPage = Page#?Page{items = NewItems, indice = NewIndice},
    add_patch([{remove, [Idx]}], delete_fix_barrier(Key, NewPage)).


insert(Key, Value, Page) ->
    #?Page{compare = CompFun, items = Items, indice = Indice} = Page,
    NewItems = erod_maps:insert(Key, Value, Items),
    {Idx, NewIndice} = erod_indices:insert(Key, Value, CompFun, Items, Indice),
    NewPage = Page#?Page{items = NewItems, indice = NewIndice},
    Size = erod_map:size(NewItems),
    add_patch([{add, [Idx], Value}],
              insert_fix_barrier(Key, Value, Idx, Size, NewPage)).


add_patch_entry(_Entry, Page) ->
    Page.


add_patch(_Patch, Page) ->
    Page.


add_patch(_Idx, [], Page) ->
    Page;

add_patch(Idx, [{remove, Path} |Patch], Page) ->
    NewEntry = {remove, [Idx| Path]},
    add_patch(Idx, Patch, add_patch_entry(NewEntry, Page));

add_patch(Idx, [{Op, Path, Value} |Patch], Page)
  when Op =:= add; Op =:= replace ->
    NewEntry = {Op, [Idx| Path], Value},
    add_patch(Idx, Patch, add_patch_entry(NewEntry, Page));

add_patch(Idx, [{Op, Path1, Path2} |Patch], Page)
  when Op =:= move; Op =:= copy ->
    NewEntry = {Op, [Idx| Path1], [Idx| Path2]},
    add_patch(Idx, Patch, add_patch_entry(NewEntry, Page)).


update_fix_barrier(Key, Value, #?Page{last = {Key, _}, first = {Key, _}} = Page) ->
    Page#?Page{first = {Key, Value}, last = {Key, Value}};

update_fix_barrier(Key, Value, #?Page{first = {Key, _}} = Page) ->
    Page#?Page{first = {Key, Value}};

update_fix_barrier(Key, Value, #?Page{last = {Key, _}} = Page) ->
    Page#?Page{last = {Key, Value}};

update_fix_barrier(_Key, _Value, Page) -> Page.


delete_fix_barrier(Key, #?Page{last = {Key, _}, first = {Key, _}} = Page) ->
    Page#?Page{first = undefined, last = undefined};

delete_fix_barrier(Key, #?Page{first = {Key, _}} = Page) ->
    #?Page{items = Items, indice = Indice} = Page,
    FirstKey = erod_indices:smallest(Indice),
    {value, FirstVal} = erod_maps:lookup(FirstKey, Items),
    Page#?Page{first = {FirstKey, FirstVal}};

delete_fix_barrier(Key, #?Page{last = {Key, _}} = Page) ->
    #?Page{items = Items, indice = Indice} = Page,
    LastKey = erod_indices:largest(Indice),
    {value, LastVal} = erod_maps:lookup(LastKey, Items),
    Page#?Page{last = {LastKey, LastVal}};

delete_fix_barrier(_Key, Page) -> Page.


insert_fix_barrier(Key, Value, 1, 1,
                   #?Page{last = undefined, first = undefined} = Page) ->
    Page#?Page{first = {Key, Value}, last = {Key, Value}};

insert_fix_barrier(Key, Value, 1, _, Page) ->
    Page#?Page{first = {Key, Value}};

insert_fix_barrier(Key, Value, Idx, Idx, Page) ->
    Page#?Page{last = {Key, Value}};

insert_fix_barrier(_Key, _Value, _Idx, _Size, Page) -> Page.
