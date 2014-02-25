-module(erod_document_view).

-export([new/2,
         from_items/4,
         insert/4,
         delete/3,
         update_inplace/5,
         update_order/5,
         commit/1,
         get_content/5]).

-define(View, ?MODULE).
-record(?View, {page_size,
                compare_fun,
                changed_pages,
                pages}).



new(PageSize, CompareFun) ->
    #?View{page_size = PageSize,
           compare_fun = CompareFun,
           changed_pages = erod_sets:new(),
           pages = erod_maps:new()}.


from_items([], PageSize, CompareFun, _Map) ->
    new(PageSize, CompareFun);

from_items(Items, PageSize, CompareFun, Map) ->
    Fun = fun({_, Va}, {_, Vb}) -> CompareFun(Va, Vb) end,
    Sorted = lists:sort(Fun, Items),
    Pages = create_pages(Sorted, PageSize, CompareFun, Map),
    #?View{page_size = PageSize,
           compare_fun = CompareFun,
           changed_pages = erod_sets:new(),
           pages = Pages}.


insert(_Key, _Value, _Map, View) ->
    View.


delete(_Key, _Map, View) ->
    View.


%%% Not changing the order
update_inplace(_Key, _Value, _Patch, _Map, View) ->
    View.


update_order(_Key, _Value, _Patch, _Map, View) ->
    View.


commit(#?View{changed_pages = ChangedPages, pages = Pages} = View) ->
    {Changed, NewPages} = commit_pages(erod_sets:to_list(ChangedPages), Pages),
    {Changed, View#?View{changed_pages = erod_sets:new(), pages = NewPages}}.


get_content(PageId, FromVer, Fun, Map, #?View{pages = Pages}) ->
    case erod_maps:lookup(PageId, Pages) of
        none -> {error, page_not_found};
        {value, Page} ->
            erod_document_page:get_content(FromVer, Fun, Map, Page)
    end.


commit_pages(Changes, Pages) ->
    commit_pages(Changes, Pages, false).


commit_pages([], Pages, Changed) ->
    {Changed, Pages};

commit_pages([Index |Rem], Pages, Changed) ->
    Page = erod_maps:value(Index, Pages),
    {PageChanged, NewPage} = erod_document_page:commit(Page),
    NewPages = erod_maps:update(Index, NewPage, Pages),
    commit_pages(Rem, NewPages, Changed or PageChanged).


create_pages(OrdValues, PageSize, CompareFun, Map) ->
    create_pages(OrdValues, PageSize, CompareFun, Map, 1, []).


create_pages([], _Size, _CompareFun, _Map, _Idx, Acc) ->
    erod_maps:from_orddict(lists:reverse(Acc));

create_pages(Values, Size, CompareFun, Map, Idx, Acc) ->
    {PageData, NewValues} = take_first_nth(Size, Values),
    Page = erod_document_page:from_ordered(PageData, CompareFun, Map),
    create_pages(NewValues, Size, CompareFun, Map, Idx + 1, [{Idx, Page} |Acc]).


take_first_nth(Size, List) ->
    take_first_nth(Size, List, []).


take_first_nth(0, List, Acc) ->
    {lists:reverse(Acc), List};

take_first_nth(_Size, [], Acc) ->
    {lists:reverse(Acc), []};

take_first_nth(Size, [Value |Rem], Acc) ->
    take_first_nth(Size - 1, Rem, [Value |Acc]).
