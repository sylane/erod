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
         get_content/3]).

-define(Page, ?MODULE).
-record(?Page, {indice,
                first,
                last,
                verlog}).


new() ->
    #?Page{indice = erod_indices:new(),
           first = undefined, last = undefined,
           verlog = erod_document_verlog:new()}.


from_ordered([], _CompareFun, _Map) -> new();

from_ordered(ValOrdDict, CompareFun, Map) ->
    Indice = erod_indices:from_ordered(ValOrdDict),
    First = hd(ValOrdDict),
    LastKey = erod_indices:largest_key(CompareFun, Map, Indice),
    Last = {LastKey, erod_maps:value(LastKey, Map)},
    #?Page{indice = Indice,
           first = First, last = Last,
           verlog = erod_document_verlog:new()}.


size(#?Page{indice = Indice}) ->
    erod_indices:size(Indice).


smallest(#?Page{first = Value}) ->
    Value.


insert(Key, Value, CompareFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    {Idx, Indice2} = erod_indices:insert(Key, Value, CompareFun, Map, Indice),
    Size = erod_indices:size(Indice2),
    Page2 = Page#?Page{indice = Indice2},
    Page3 = insert_fix_barrier(Value, Idx, Size, Page2),
    add_patch({add, [Idx], Value}, Page3).


delete(Key, CompareFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    Size = erod_indices:size(Indice),
    {Idx, Indice2} = erod_indices:delete(Key, CompareFun, Map, Indice),
    Page2 = Page#?Page{indice = Indice2},
    Page3 = delete_fix_barrier(Idx, Size, CompareFun, Map, Page2),
    add_patch({remove, [Idx]}, Page3).


%%% Not changing the order
update_inplace(Key, Value, Patch, CompareFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    Idx = erod_indices:index(Key, Value, CompareFun, Map, Indice),
    Size = erod_indices:size(Indice),
    Page2 = update_fix_barrier(Value, Idx, Idx, Size, CompareFun, Map, Page),
    add_patch(Idx, Patch, Page2).


update_order(Key, Value, Patch, CompareFun, Map, Page) ->
    #?Page{indice = Indice} = Page,
    case is_local(Value, CompareFun, Page) of
        false ->
            Size = erod_indices:size(Indice),
            {Idx, Indice2} = erod_indices:delete(Key, Value, CompareFun, Map, Indice),
            Page2 = Page#?Page{indice = Indice2},
            Page3 = delete_fix_barrier(Idx, Size, CompareFun, Map, Page2),
            {deleted, add_patch([{remove, [Idx]}], Page3)};
        true ->
            {From, Indice2} = erod_indices:delete(Key, Value, CompareFun, Map, Indice),
            {To, Indice3} = erod_indices:insert(Key, Value, CompareFun, Map, Indice2),
            Size = erod_indices:size(Indice3),
            Page2 = Page#?Page{indice = Indice3},
            Page3 = update_fix_barrier(Value, From, To, Size, CompareFun, Map, Page2),
            {ok, add_patch({move, [From], [To]}, add_patch(From, Patch, Page3))}
    end.


commit(#?Page{verlog = VerLog} = Page) ->
    {Changed, NewVerLog} = erod_document_verlog:commit(VerLog),
    {Changed, Page#?Page{verlog = NewVerLog}}.


get_content(FromVer, Map, #?Page{indice = Indice, verlog = VerLog}) ->
    case erod_document_verlog:get_patch(FromVer, VerLog) of
        none ->
            Version = erod_document_verlog:version(VerLog),
            {page, Version, erod_indices:values(Map, Indice)};
        Patch -> Patch
    end.


add_patch(Patch, #?Page{verlog = VerLog} = Page) ->
    Page#?Page{verlog = erod_document_verlog:add_patch(Patch, VerLog)}.


add_patch(Prefix, Patch, #?Page{verlog = VerLog} = Page) ->
    Page#?Page{verlog = erod_document_verlog:add_patch(Prefix, Patch, VerLog)}.


is_local(Value, CompareFun, #?Page{first = First, last = Last}) ->
    (not CompareFun(Value, First)) andalso CompareFun(Value, Last).


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

delete_fix_barrier(1, _Size, CompareFun, Map, Page) ->
    % The first value has been delete
    NewFirst = erod_indices:smallest_value(CompareFun, Map, Page#?Page.indice),
    Page#?Page{first = NewFirst};

delete_fix_barrier(From, From, CompareFun, Map, Page) ->
    % The last value has been delete
    NewLast = erod_indices:largest_value(CompareFun, Map, Page#?Page.indice),
    Page#?Page{last = NewLast};

delete_fix_barrier(_From, _Sie, _CompareFun,_Map, Page) ->
    % The change is not relevent for the barrier
    Page.


%%% Size AFTER insertion
insert_fix_barrier(Value, 1, 1, Page) ->
    % The first value to be inserted
    Page#?Page{first = Value, last = Value};

insert_fix_barrier(Value, 1, _, Page) ->
    % The value has been inserted at the first position
    Page#?Page{first = Value};

insert_fix_barrier(Value, Idx, Idx, Page) ->
    % The value has been inserted at the last position
    Page#?Page{last = Value};

insert_fix_barrier(_Value, _Idx, _Size, Page) ->
    % The value position is not relevent for the barrier
    Page.


update_fix_barrier(Value, 1, 1, 1, _CompareFun, _Map, Page) ->
    % The only value changed
    Page#?Page{first = Value, last = Value};

update_fix_barrier(Value, 1, 1, _Size, _CompareFun, _Map, Page) ->
    % The first value changed
    Page#?Page{first = Value};

update_fix_barrier(Value, Idx, Idx, Idx, _CompareFun, _Map, Page) ->
    % The last value changed
    Page#?Page{last = Value};

update_fix_barrier(_Value, Idx, Idx, _Size, _CompareFun, _Map, Page) ->
    % The value index did not change and it is neither first or last
    Page;

update_fix_barrier(Value, 1, To, To, CompareFun, Map, Page) ->
    % The value moved from the first position to the last position
    NewFirst = erod_indices:smallest_value(CompareFun, Map, Page#?Page.indice),
    Page#?Page{first = NewFirst, last = Value};

update_fix_barrier(Value, To, 1, To, CompareFun, Map, Page) ->
    % The value moved from the last position to the first position
    NewLast = erod_indices:largest_value(CompareFun, Map, Page#?Page.indice),
    Page#?Page{first = Value, last = NewLast};

update_fix_barrier(Value, _From, To, To, _CompareFun, _Map, Page) ->
    % The value became to the last one
    Page#?Page{last = Value};

update_fix_barrier(_Value, From, _To, From, CompareFun, Map, Page) ->
    % The value is not the last one anymore
    NewLast = erod_indices:largest_value(CompareFun, Map, Page#?Page.indice),
    Page#?Page{last = NewLast};

update_fix_barrier(Value, _From, 1, _Size, _CompareFun, _Map, Page) ->
    % The value became the first one
    Page#?Page{first = Value};

update_fix_barrier(_Value, 1, _To, _Size, CompareFun, Map, Page) ->
    % The value is not the first one anymore
    NewFirst = erod_indices:smallest_value(CompareFun, Map, Page#?Page.indice),
    Page#?Page{first = NewFirst};

update_fix_barrier(_Value, _From, _To, _Size, _CompareFun, _Map, Page) ->
    % Nothing relevent for the barrier
    Page.
