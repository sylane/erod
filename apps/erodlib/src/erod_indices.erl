-module(erod_indices).

-export([new/0,
         from_indice/1,
         from_ordered/1,
         for_map/2,
         keys/1,
         values/2,
         to_list/2,
         map/3,
         size/1,
         index/4, index/5,
         insert/4, insert/5,
         delete/4, delete/5,
         smallest_key/3,
         smallest_value/3,
         take_smallest_key/3,
         take_smallest_value/3,
         take_smallest_keys/4,
         largest_key/3,
         largest_value/3,
         take_largest_key/3,
         take_largest_value/3,
         take_largest_keys/4]).


-define(pow(A), A * A).

-define(div2(X), X bsr 1).

-define(mul2(X), X bsl 1).


new() ->
    {?MODULE, nil}.


from_indice(Keys) ->
    {?MODULE, balance_list(Keys, length(Keys))}.


from_ordered(Items) ->
    Keys = [K || {K, _} <- Items],
    {?MODULE, balance_list(Keys, length(Keys))}.


for_map(CompareFun, Map) ->
    Fun = fun(Ka, Ka) -> true;
             (Ka, Kb) -> CompareFun(erod_maps:value(Ka, Map),
                                    erod_maps:value(Kb, Map))
          end,
    from_indice(lists:usort(Fun, erod_maps:keys(Map))).


keys({?MODULE, Tree}) ->
    tree_to_keys(Tree).


values(Map, {?MODULE, Tree}) ->
    tree_to_values(Map, Tree).


to_list(Map, {?MODULE, Tree}) ->
    tree_to_list(Map, Tree).


map(Fun, Map, {?MODULE, Tree}) ->
    tree_map(Fun, Map, Tree).


size({?MODULE, Tree}) ->
    tree_size(Tree).


index(Key, CompareFun, Map, Indice) ->
    index(Key, erod_maps:value(Key, Map), CompareFun, Map, Indice).


index(Key, Value, CompareFun, Map, {?MODULE, Tree}) ->
    tree_index(Key, Value, CompareFun, Map, Tree).


insert(Key, CompareFun, Map, Indice) ->
    insert(Key, erod_maps:value(Key, Map), CompareFun, Map, Indice).


insert(Key, Value, CompareFun, Map, {?MODULE, Tree}) ->
    Size = tree_size(Tree),
    WeightRef = ?pow(Size + 1),
    {_, Idx, NewTree} = tree_insert(Key, Value, CompareFun, Map,
                                    Tree, WeightRef),
    {Idx, {?MODULE, NewTree}}.


delete(Key, CompareFun, Map, Indice) ->
    delete(Key, erod_maps:value(Key, Map), CompareFun, Map, Indice).


delete(Key, Value, CompareFun, Map, {?MODULE, Tree}) ->
    {Idx, NewTree} = tree_delete(Key, Value, CompareFun, Map, Tree),
    {Idx, {?MODULE, NewTree}}.


smallest_key(_ComapreFun, _Map, {?MODULE, Tree}) ->
    tree_smallest(Tree).


smallest_value(_CompareFun, Map, {?MODULE, Tree}) ->
    erod_maps:value(tree_smallest(Tree), Map).


take_smallest_key(_ComapreFun, _Map, {?MODULE, Tree}) ->
    {SmallestKey, NewTree} = tree_take_smallest(Tree),
    {SmallestKey, {?MODULE, NewTree}}.


take_smallest_value(_ComparFun, Map, {?MODULE, Tree}) ->
    {SmallestKey, NewTree} = tree_take_smallest(Tree),
    {erod_maps:value(SmallestKey, Map), {?MODULE, NewTree}}.


take_smallest_keys(_Count, _ComapreFun, _Map, {?MODULE, Tree}) ->
    %TODO
    {[], {?MODULE, Tree}}.


largest_key(_CompareFun, _Map, {?MODULE, Tree}) ->
    tree_largest(Tree).


largest_value(_CompareFun, Map, {?MODULE, Tree}) ->
    erod_maps:value(tree_largest(Tree), Map).


take_largest_key(_ComapreFun, _Map, {?MODULE, Tree}) ->
    {LargestKey, NewTree} = tree_take_largest(Tree),
    {LargestKey, {?MODULE, NewTree}}.


take_largest_value(_CompareFun, Map, {?MODULE, Tree}) ->
    {LargestKey, NewTree} = tree_take_largest(Tree),
    {erod_maps:value(LargestKey, Map), {?MODULE, NewTree}}.


take_largest_keys(_Count, _ComapreFun, _Map, {?MODULE, Tree}) ->
    %TODO
    {[], {?MODULE, Tree}}.


%%% Internal

tree_to_keys(Tree) -> tree_to_keys(Tree, []).


tree_to_keys({Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    tree_to_keys(Smaller, [Key |tree_to_keys(Larger, Acc)]);

tree_to_keys(nil, Acc) -> Acc.


tree_to_values(Map, Tree) -> tree_to_values(Map, Tree, []).


tree_to_values(Map, {Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    Value = erod_maps:value(Key, Map),
    tree_to_values(Map, Smaller, [Value |tree_to_values(Map, Larger, Acc)]);

tree_to_values(_Map, nil, Acc) -> Acc.


tree_to_list(Map, Tree) -> tree_to_list(Map, Tree, []).


tree_to_list(Map, {Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    Item = {Key, erod_maps:value(Key, Map)},
    tree_to_list(Map, Smaller, [Item |tree_to_list(Map, Larger, Acc)]);

tree_to_list(_Map, nil, Acc) -> Acc.


tree_map(Fun, Map, Tree) -> tree_map(Fun, Map, Tree, []).


tree_map(Fun, Map, {Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    Result = Fun({Key, erod_maps:value(Key, Map)}),
    tree_map(Fun, Map, Smaller, [Result |tree_map(Fun, Map, Larger, Acc)]);

tree_map(_Fun, _Map, nil, Acc) -> Acc.


tree_size(nil) -> 0;

tree_size({_Key, Size, _Weight, _Smaller, _Larger}) -> Size.


tree_index(RefKey, _Val, _Comp, _Map, {RefKey, _S, _W, Smaller, _Larger}) ->
    tree_size(Smaller) + 1;

tree_index(Key, Val, Comp, Map, {RefKey, _S, _W, Smaller, Larger}) ->
    case Comp(Val, erod_maps:value(RefKey, Map)) of
        true ->
            tree_index(Key, Val, Comp, Map, Smaller);
        false ->
            tree_size(Smaller) + 1 + tree_index(Key, Val, Comp, Map, Larger)
    end.


tree_insert(RefKey, _Val, _Comp, _Map, {RefKey, _S, _W, _Sm, _Lg}, _WRef) ->
    erlang:error({key_already_exists, RefKey});

tree_insert(Key, Val, Comp, Map, {RefKey, _S, _W, Smaller, Larger}, WRef) ->
    case Comp(Val, erod_maps:value(RefKey, Map)) of
        true ->
            case tree_insert(Key, Val, Comp, Map, Smaller, ?div2(WRef)) of
                {ok, Idx, NewSmaller} ->
                    {ok, Idx, combinate(RefKey, NewSmaller, Larger)};
                {check, Idx, NewSmaller} ->
                    NewTree = combinate(RefKey, NewSmaller, Larger),
                    {_, Size, Weight, _, _} = NewTree,
                    NewWRef = ?pow(Size),
                    if Weight > NewWRef -> {ok, Idx, balance_tree(NewTree)};
                       true -> {check, Idx, NewTree}
                    end
            end;
        false ->
            case tree_insert(Key, Val, Comp, Map, Larger, ?div2(WRef)) of
                {ok, Idx, NewLarger} ->
                    NewIdx = tree_size(Smaller) + Idx + 1,
                    {ok, NewIdx, combinate(RefKey, Smaller, NewLarger)};
                {check, Idx, NewLarger} ->
                    NewIdx = tree_size(Smaller) + Idx + 1,
                    NewTree = combinate(RefKey, Smaller, NewLarger),
                    {_, Size, Weight, _, _} = NewTree,
                    NewWRef = ?pow(Size),
                    if Weight > NewWRef -> {ok, NewIdx, balance_tree(NewTree)};
                       true -> {check, NewIdx, NewTree}
                    end
            end
    end;

tree_insert(Key, _Val, _Comp, _Map, nil, 0) ->
    {check, 1, combinate(Key, nil, nil)};

tree_insert(Key, _Val, _Comp, _Map, nil, _) ->
    {ok, 1, combinate(Key, nil, nil)}.


tree_delete(RefKey, _Val, _Comp, _Map, {RefKey, _S, _W, Smaller, Larger}) ->
    {tree_size(Smaller) + 1, tree_merge(Smaller, Larger)};

tree_delete(Key, Val, Comp, Map, {RefKey, _Size, _Weight, Smaller, Larger}) ->
    case Comp(Val, erod_maps:value(RefKey, Map)) of
        true ->
            {Idx, NewSmaller} = tree_delete(Key, Val, Comp, Map, Smaller),
            {Idx, combinate(RefKey, NewSmaller, Larger)};
        false ->
            {Idx, NewBigger} = tree_delete(Key, Val, Comp, Map, Larger),
            NewIdx = tree_size(Smaller) + Idx + 1,
            {NewIdx, combinate(RefKey, Smaller, NewBigger)}
    end.


tree_merge(Smaller, nil) -> Smaller;

tree_merge(nil, Larger) -> Larger;

tree_merge(Smaller, Larger) ->
    {Key, NewLarger} = tree_take_smallest(Larger),
    combinate(Key, Smaller, NewLarger).


tree_smallest({Key, _Size, _Weight, nil, _Larger}) ->
    Key;

tree_smallest({_Key, _Size, _Weight, Smaller, _Larger}) ->
    tree_smallest(Smaller).


tree_take_smallest({Key, _Size, _Weight, nil, Larger}) ->
    {Key, Larger};

tree_take_smallest({Key, _Size, _Weight, Smaller, Larger}) ->
    {SmallestValue, NewSmaller} = tree_take_smallest(Smaller),
    {SmallestValue, combinate(Key, NewSmaller, Larger)}.


tree_largest({Key, _Size, _Weight, _Smaller, nil}) ->
    Key;

tree_largest({_Key, _Size, _Weight, _Smaller, Larger}) ->
    tree_largest(Larger).


tree_take_largest({Key, _Size, _Weight, Smaller, nil}) ->
    {Key, Smaller};

tree_take_largest({Key, _Size, _Weight, Smaller, Larger}) ->
    {LargestValue, NewLarger} = tree_take_largest(Larger),
    {LargestValue, combinate(Key, Smaller, NewLarger)}.


balance_tree({_Key, Size, _Weight, _Smaller, _Larger} = Tree) ->
    balance_list(tree_to_keys(Tree), Size).


balance_list(Keys, Size) ->
    {Tree, []} = balance_branch(Keys, Size),
    Tree.


balance_branch(Keys, Size) when Size > 1 ->
    BothSize = Size - 1,
    LargerSize = BothSize div 2,
    SmallerSize = BothSize - LargerSize,
    {Smaller, [Pivot | Remaining1]} = balance_branch(Keys, SmallerSize),
    {Larger, Remaining2} = balance_branch(Remaining1, LargerSize),
    {combinate(Pivot, Smaller, Larger), Remaining2};

balance_branch([Key |Keys], 1) ->
    {combinate(Key, nil, nil), Keys};

balance_branch(Keys, 0) ->
    {nil, Keys}.


combinate(K, {_K1, S1, H1, _Sm1, _Lg1} = Sm, {_K2, S2, H2, _Sm2, _Lg2} = Lg) ->
    {K, S1 + S2 + 1, ?mul2(erlang:max(H1, H2)), Sm, Lg};

combinate(K, {_K1, S1, H1, _Sm1, _Lg1} = Sm, nil) ->
    {K, S1 + 1, ?mul2(H1), Sm, nil};

combinate(K, nil, {_K2, S2, H2, _Sm2, _Lg2} = Lg) ->
    {K, S2 + 1, ?mul2(H2), nil, Lg};

combinate(K, nil , nil) ->
    {K, 1, 1, nil, nil}.

