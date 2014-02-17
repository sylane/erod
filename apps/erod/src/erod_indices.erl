-module(erod_indices).

-export([new/0,
         from_list/2,
         from_ordset/1,
         to_list/1,
         size/1,
         index/3,
         insert/3,
         delete/3,
         smallest/1,
         take_smallest/1,
         largest/1,
         take_largest/1,
         balance/1]).


-define(pow(A), A * A).

-define(div2(X), X bsr 1).

-define(mul2(X), X bsl 1).


new() ->
    {?MODULE, nil}.


from_list(Values, OrderFun) ->
    from_ordset(lists:usort(OrderFun, Values)).


from_ordset(Values) ->
    {?MODULE, balance_list(Values, length(Values))}.


to_list({?MODULE, Tree}) ->
    tree_to_list(Tree, []).


size({?MODULE, Tree}) ->
    tree_size(Tree).


index(Value, OrderFun, {?MODULE, Tree}) ->
    tree_index(Value, OrderFun, Tree).


insert(Value, OrderFun, {?MODULE, Tree}) ->
    Size = tree_size(Tree),
    WeightRef = ?pow(Size + 1),
    {_, Idx, NewTree} = tree_insert(Value, OrderFun, Tree, WeightRef),
    {Idx, {?MODULE, NewTree}}.


delete(Value, OrderFun, {?MODULE, Tree}) ->
    {Idx, NewTree} = tree_delete(Value, OrderFun, Tree),
    {Idx, {?MODULE, NewTree}}.


smallest({?MODULE, Tree}) ->
    tree_smallest(Tree).


take_smallest({?MODULE, Tree}) ->
    {SmallestValue, NewTree} = tree_take_smallest(Tree),
    {SmallestValue, {?MODULE, NewTree}}.


largest({?MODULE, Tree}) ->
    tree_largest(Tree).


take_largest({?MODULE, Tree}) ->
    {Largestalue, NewTree} = tree_take_largest(Tree),
    {Largestalue, {?MODULE, NewTree}}.


balance({?MODULE, Tree}) ->
    {?MODULE, balance_tree(Tree)}.


%%% Internal

tree_to_list(Tree) -> tree_to_list(Tree, []).


tree_to_list({Value, _Size, _Weight, Smaller, Larger}, Acc) ->
    tree_to_list(Smaller, [Value | tree_to_list(Larger, Acc)]);

tree_to_list(nil, Acc) -> Acc.


tree_size(nil) -> 0;

tree_size({_Value, Size, _Weight, _Smaller, _Larger}) -> Size.


tree_index(RefVal, _OrderFun, {RefVal, _Size, _Weight, Smaller, _Larger}) ->
    tree_size(Smaller) + 1;

tree_index(Value, OrderFun, {RefVal, _Size, _Weight, Smaller, Larger}) ->
    case OrderFun(Value, RefVal) of
        true ->
            tree_index(Value, OrderFun, Smaller);
        false ->
            tree_size(Smaller) + 1 + tree_index(Value, OrderFun, Larger)
    end.


tree_insert(RefVal, _OrderFun, {RefVal, _Size, _Weight, _Sm, _Lg}, _WRef) ->
    erlang:error({key_already_exists, RefVal});

tree_insert(Value, OrderFun, {RefVal, _S, _W, Smaller, Larger}, WRef) ->
    case OrderFun(Value, RefVal) of
        true ->
            case tree_insert(Value, OrderFun, Smaller, ?div2(WRef)) of
                {ok, Idx, NewSmaller} ->
                    {ok, Idx, combinate(RefVal, NewSmaller, Larger)};
                {check, Idx, NewSmaller} ->
                    NewTree = combinate(RefVal, NewSmaller, Larger),
                    {_, Size, Weight, _, _} = NewTree,
                    NewWRef = ?pow(Size),
                    if Weight > NewWRef -> {ok, Idx, balance_tree(NewTree)};
                       true -> {check, Idx, NewTree}
                    end
            end;
        false ->
            case tree_insert(Value, OrderFun, Larger, ?div2(WRef)) of
                {ok, Idx, NewLarger} ->
                    NewIdx = tree_size(Smaller) + Idx + 1,
                    {ok, NewIdx, combinate(RefVal, Smaller, NewLarger)};
                {check, Idx, NewLarger} ->
                    NewIdx = tree_size(Smaller) + Idx + 1,
                    NewTree = combinate(RefVal, Smaller, NewLarger),
                    {_, Size, Weight, _, _} = NewTree,
                    NewWRef = ?pow(Size),
                    if Weight > NewWRef -> {ok, NewIdx, balance_tree(NewTree)};
                       true -> {check, NewIdx, NewTree}
                    end
            end
    end;

tree_insert(Value, _OrderFun, nil, 0) ->
    {check, 1, combinate(Value, nil, nil)};

tree_insert(Value, _OrderFun, nil, _) ->
    {ok, 1, combinate(Value, nil, nil)}.


tree_delete(Value, _OrderFun, {Value, _Size, _Weight, Smaller, Larger}) ->
    {tree_size(Smaller) + 1, tree_merge(Smaller, Larger)};

tree_delete(Value, OrderFun, {RefVal, _Size, _Weight, Smaller, Larger}) ->
    case OrderFun(Value, RefVal) of
        true ->
            {Idx, NewSmaller} = tree_delete(Value, OrderFun, Smaller),
            {Idx, combinate(RefVal, NewSmaller, Larger)};
        false ->
            {Idx, NewBigger} = tree_delete(Value, OrderFun, Larger),
            NewIdx = tree_size(Smaller) + Idx + 1,
            {NewIdx, combinate(RefVal, Smaller, NewBigger)}
    end.


tree_merge(Smaller, nil) -> Smaller;

tree_merge(nil, Larger) -> Larger;

tree_merge(Smaller, Larger) ->
    {Value, NewLarger} = tree_take_smallest(Larger),
    combinate(Value, Smaller, NewLarger).


tree_smallest({Value, _Size, _Weight, nil, _Larger}) ->
    Value;

tree_smallest({_Value, _Size, _Weight, Smaller, _Larger}) ->
    tree_smallest(Smaller).


tree_take_smallest({Value, _Size, _Weight, nil, Larger}) ->
    {Value, Larger};

tree_take_smallest({Value, _Size, _Weight, Smaller, Larger}) ->
    {SmallestValue, NewSmaller} = tree_take_smallest(Smaller),
    {SmallestValue, combinate(Value, NewSmaller, Larger)}.


tree_largest({Value, _Size, _Weight, _Smaller, nil}) ->
    Value;

tree_largest({_Value, _Size, _Weight, _Smaller, Larger}) ->
    tree_largest(Larger).


tree_take_largest({Value, _Size, _Weight, Smaller, nil}) ->
    {Value, Smaller};

tree_take_largest({Value, _Size, _Weight, Smaller, Larger}) ->
    {LargestValue, NewLarger} = tree_take_largest(Larger),
    {LargestValue, combinate(Value, Smaller, NewLarger)}.


balance_tree({_Value, Size, _Weight, _Smaller, _Larger} = Tree) ->
    balance_list(tree_to_list(Tree), Size).


balance_list(Values, Size) ->
    {Tree, []} = balance_branch(Values, Size),
    Tree.


balance_branch(Values, Size) when Size > 1 ->
    BothSize = Size - 1,
    LargerSize = BothSize div 2,
    SmallerSize = BothSize - LargerSize,
    {Smaller, [Pivot | Remaining1]} = balance_branch(Values, SmallerSize),
    {Larger, Remaining2} = balance_branch(Remaining1, LargerSize),
    {combinate(Pivot, Smaller, Larger), Remaining2};

balance_branch([Value |Values], 1) ->
    {combinate(Value, nil, nil), Values};

balance_branch(Values, 0) ->
    {nil, Values}.


combinate(V, {_V1, S1, H1, _Sm1, _Lg1} = Sm, {_V2, S2, H2, _Sm2, _Lg2} = Lg) ->
    {V, S1 + S2 + 1, ?mul2(erlang:max(H1, H2)), Sm, Lg};

combinate(V, {_V1, S1, H1, _Sm1, _Lg1} = Sm, nil) ->
    {V, S1 + 1, ?mul2(H1), Sm, nil};

combinate(V, nil, {_V2, S2, H2, _Sm2, _Lg2} = Lg) ->
    {V, S2 + 1, ?mul2(H2), nil, Lg};

combinate(V, nil , nil) ->
    {V, 1, 1, nil, nil}.

