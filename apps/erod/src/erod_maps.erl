-module(erod_maps).

-export([new/0,
         from_orddict/1,
         from_items/1,
         size/1,
         keys/1,
         lookup/2,
         value/2,
         insert/3,
         update/3,
         delete/2,
         lookup_from/2,
         trim_smallest/2]).


new() ->
    gb_trees:empty().


from_orddict(OrdDict) ->
    gb_trees:from_orddict(OrdDict).


from_items(Items) ->
    gb_trees:from_orddict(orddict:from_list(Items)).


size(Map) ->
    gb_trees:size(Map).


keys(Map) ->
    gb_trees:keys(Map).


lookup(Key, Map) ->
    gb_trees:lookup(Key, Map).


value(Key, Map) ->
    {value, Value} = gb_trees:lookup(Key, Map),
    Value.


insert(Key, Value, Map) ->
    gb_trees:insert(Key, Value, Map).


update(Key, Value, Map) ->
    gb_trees:update(Key, Value, Map).


delete(Key, Map) ->
    gb_trees:delete(Key, Map).


lookup_from(Key, {_Size, Tree}) ->
    case tree_lookup_from(Key, Tree) of
        none -> none;
        Values -> {values, lists:reverse(Values)}
    end.


trim_smallest(MinSize, {Size, Tree}) when Size > MinSize ->
    %TODO
    {Size, Tree};

trim_smallest(_MinSize, Map) -> Map.


tree_lookup_from(Key, {Key1, Value, Smaller, Bigger}) when Key < Key1 ->
    tree_collect(Value, tree_lookup_from(Key, Smaller), Bigger);

tree_lookup_from(Key, {Key1, _Value, _Smaller, Bigger}) when Key > Key1 ->
    tree_lookup_from(Key, Bigger);

tree_lookup_from(_Key, {_Key1, Value, _Smaller, Bigger}) ->
    tree_collect([Value], Bigger);

tree_lookup_from(_Key, nil) -> none.


tree_collect(_Value, none, _Tree) -> none;

tree_collect(Value, Acc, nil) -> [Value |Acc];

tree_collect(Value1, Acc, {_Key, Value2, Smaller, Bigger}) ->
    tree_collect(Value2, tree_collect(Value1, Acc, Smaller), Bigger).


tree_collect(Acc, nil) -> Acc;

tree_collect(Acc, {_Key, Value, Smaller, Bigger}) ->
    tree_collect(Value, tree_collect(Acc, Smaller), Bigger).
