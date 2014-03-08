%%% Requirments:
%%%    - Fast Item lookup by key
%%%    - Ordered by given comparison function
%%%    - Relatively fast insert/delete/update by key
%%%    - Optional reordering on update.
%%%    - Keep track of the version
%%%    - Keep a log of changes in a JSON patch-like format.

-module(erod_document_page).

-export([new/0,
         from_ordered/3,
         size/1,
         smallest/1,
         insert/5,
         delete/4,
         update_inplace/6,
         update_order/6,
         commit/1,
         get_content/4]).

-define(Page, ?MODULE).
-record(?Page, {indice,
                first,
                last,
                verlog}).


new() ->
    #?Page{indice = erodlib_indices:new(),
           first = undefined, last = undefined,
           verlog = erod_document_verlog:new()}.


from_ordered([], _CompareFun, _Map) -> new();

from_ordered(ValOrdDict, CompFun, Map) ->
    Indice = erodlib_indices:from_ordered(ValOrdDict),
    {_, First} = hd(ValOrdDict),
    Last = erodlib_indices:largest_value(CompFun, Map, Indice),
    #?Page{indice = Indice,
           first = First, last = Last,
           verlog = erod_document_verlog:new()}.


size(#?Page{indice = Indice}) ->
    erodlib_indices:size(Indice).


smallest(#?Page{first = Val}) ->
    Val.


insert(Key, Val, CompFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    {Idx, Indice2} = erodlib_indices:insert(Key, Val, CompFun, Map, Indice),
    Size = erodlib_indices:size(Indice2),
    Page2 = Page#?Page{indice = Indice2},
    Page3 = insert_fix_barrier(Val, Idx, Size, Page2),
    add_patch({add, [Idx], Val}, Page3).


delete(Key, CompFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    Size = erodlib_indices:size(Indice),
    {Idx, Indice2} = erodlib_indices:delete(Key, CompFun, Map, Indice),
    Page2 = Page#?Page{indice = Indice2},
    Page3 = delete_fix_barrier(Idx, Size, CompFun, Map, Page2),
    add_patch({remove, [Idx]}, Page3).


%%% Not changing the order
update_inplace(Key, Val, Patch, CompFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    Idx = erodlib_indices:index(Key, Val, CompFun, Map, Indice),
    Size = erodlib_indices:size(Indice),
    Page2 = update_fix_barrier(Val, Idx, Idx, Size, CompFun, Map, Page),
    add_patch(Idx, Patch, Page2).


update_order(Key, Val, Patch, CompFun, Map, Page) ->
    #?Page{indice = Idx} = Page,
    case is_local(Val, CompFun, Page) of
        false ->
            Size = erodlib_indices:size(Idx),
            {I, Idx2} = erodlib_indices:delete(Key, Val, CompFun, Map, Idx),
            Page2 = Page#?Page{indice = Idx2},
            Page3 = delete_fix_barrier(I, Size, CompFun, Map, Page2),
            {deleted, add_patch([{remove, [I]}], Page3)};
        true ->
            {I1, Idx2} = erodlib_indices:delete(Key, Val, CompFun, Map, Idx),
            {I2, Idx3} = erodlib_indices:insert(Key, Val, CompFun, Map, Idx2),
            Size = erodlib_indices:size(Idx3),
            Page2 = Page#?Page{indice = Idx3},
            Page3 = update_fix_barrier(Val, I1, I2, Size, CompFun, Map, Page2),
            {ok, add_patch({move, [I1], [I2]}, add_patch(I1, Patch, Page3))}
    end.


commit(#?Page{verlog = VerLog} = Page) ->
    {Changed, NewVerLog} = erod_document_verlog:commit(VerLog),
    {Changed, Page#?Page{verlog = NewVerLog}}.


get_content(FromVer, Fun, Map, #?Page{indice = Indice, verlog = VerLog}) ->
    PageSize = erodlib_indices:size(Indice),
    TotalSize = erodlib_maps:size(Map),
    case erod_document_verlog:get_patch(FromVer, VerLog) of
        unchanged -> unchanged;
        none ->
            Version = erod_document_verlog:version(VerLog),
            PageData = erodlib_indices:map(Fun, Map, Indice),
            {entity, Version, PageSize, TotalSize, PageData};
        {patch, Version, Patch} ->
            {patch, Version, PageSize, TotalSize, Patch}
    end.


add_patch(Patch, #?Page{verlog = VerLog} = Page) ->
    Page#?Page{verlog = erod_document_verlog:add_patch(Patch, VerLog)}.


add_patch(Prefix, Patch, #?Page{verlog = VerLog} = Page) ->
    Page#?Page{verlog = erod_document_verlog:add_patch(Prefix, Patch, VerLog)}.


is_local(Val, CompFun, #?Page{first = First, last = Last}) ->
    (not CompFun(Val, First)) andalso CompFun(Val, Last).


%%% Size BEFORE deletion
delete_fix_barrier(1, 1, _CompareFun, _Map, Page) ->
    % The last value has been deleted
    Page#?Page{first = undefined, last = undefined};

delete_fix_barrier(1, 2, _CompareFun,_Map, Page) ->
    % The first value has been deleted and there is only one left
    Page#?Page{first = Page#?Page.last};

delete_fix_barrier(2, 2, _CompareFun,_Map, Page) ->
    % The last value has been deleted and there is only one left
    Page#?Page{last = Page#?Page.first};

delete_fix_barrier(1, _Size, CompFun, Map, Page) ->
    % The first value has been delete
    NewFirst = erodlib_indices:smallest_value(CompFun, Map, Page#?Page.indice),
    Page#?Page{first = NewFirst};

delete_fix_barrier(From, From, CompFun, Map, Page) ->
    % The last value has been delete
    NewLast = erodlib_indices:largest_value(CompFun, Map, Page#?Page.indice),
    Page#?Page{last = NewLast};

delete_fix_barrier(_From, _Sie, _CompareFun,_Map, Page) ->
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


update_fix_barrier(Val, 1, 1, 1, _CompareFun, _Map, Page) ->
    % The only value changed
    Page#?Page{first = Val, last = Val};

update_fix_barrier(Val, 1, 1, _Size, _CompareFun, _Map, Page) ->
    % The first value changed
    Page#?Page{first = Val};

update_fix_barrier(Val, Idx, Idx, Idx, _CompareFun, _Map, Page) ->
    % The last value changed
    Page#?Page{last = Val};

update_fix_barrier(_Value, Idx, Idx, _Size, _CompareFun, _Map, Page) ->
    % The value index did not change and it is neither first or last
    Page;

update_fix_barrier(Val, 1, To, To, CompFun, Map, Page) ->
    % The value moved from the first position to the last position
    NewFirst = erodlib_indices:smallest_value(CompFun, Map, Page#?Page.indice),
    Page#?Page{first = NewFirst, last = Val};

update_fix_barrier(Val, To, 1, To, CompFun, Map, Page) ->
    % The value moved from the last position to the first position
    NewLast = erodlib_indices:largest_value(CompFun, Map, Page#?Page.indice),
    Page#?Page{first = Val, last = NewLast};

update_fix_barrier(Val, _From, To, To, _CompareFun, _Map, Page) ->
    % The value became to the last one
    Page#?Page{last = Val};

update_fix_barrier(_Value, From, _To, From, CompFun, Map, Page) ->
    % The value is not the last one anymore
    NewLast = erodlib_indices:largest_value(CompFun, Map, Page#?Page.indice),
    Page#?Page{last = NewLast};

update_fix_barrier(Val, _From, 1, _Size, _CompareFun, _Map, Page) ->
    % The value became the first one
    Page#?Page{first = Val};

update_fix_barrier(_Value, 1, _To, _Size, CompFun, Map, Page) ->
    % The value is not the first one anymore
    NewFirst = erodlib_indices:smallest_value(CompFun, Map, Page#?Page.indice),
    Page#?Page{first = NewFirst};

update_fix_barrier(_Value, _From, _To, _Size, _CompareFun, _Map, Page) ->
    % Nothing relevent for the barrier
    Page.
