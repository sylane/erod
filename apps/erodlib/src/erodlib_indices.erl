%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erodlib.
%%%
%%% Erodlib is free software: you can redistribute it and/or modify
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
%%% @doc TODO: Document module erodlib_indices.
%%% @end
%%% ==========================================================================

-module(erodlib_indices).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/0,
         from_ordset/1,
         from_orddict/1,
         for_map/2,
         keys/1,
         values/2,
         items/2,
         map/3,
         size/1,
         position/4, position/5,
         insert/4, insert/5,
         delete/4, delete/5,
         smallest_key/1,
         smallest_value/2,
         take_smallest_key/1,
         take_smallest_value/2,
         take_smallest_keys/2,
         largest_key/1,
         largest_value/2,
         take_largest_key/1,
         take_largest_value/2,
         take_largest_keys/2]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(pow(A), A * A).

-define(div2(X), X bsr 1).

-define(mul2(X), X bsl 1).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

% It is not possible to specify the indice's node because some functions
% do some assumptions, like the indice not beeing empty, and that would make
% dialyzer fail with an "invalid type specification". To fix this the
% functions like delete/4 should not assume the map has a root node.
%-type node() :: nil | {term(), pos_integer(), pos_integer(), node(), node()}.
-type indice() :: {?MODULE, term()}.
-type compare_fun() :: fun((term(), term()) -> boolean()).
-type map_fun() :: fun(({term(), term()}) -> term()).
-export_type([indice/0, compare_fun/0, map_fun/0]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new empty indice.
%% @end
%% -----------------------------------------------------------------
-spec new() -> Indice
    when Indice :: indice().
%% -----------------------------------------------------------------

new() ->
    {?MODULE, nil}.


%% -----------------------------------------------------------------
%% @doc Creates a new indice from an ordered list of keys.
%% @end
%% -----------------------------------------------------------------
-spec from_ordset(Keys) -> Indice
    when Keys :: [term()], Indice :: indice().
%% -----------------------------------------------------------------

from_ordset(Keys) ->
    {?MODULE, balance_list(Keys, length(Keys))}.


%% -----------------------------------------------------------------
%% @doc Creates a new indice from an ordered list of items.
%% @end
%% -----------------------------------------------------------------
-spec from_orddict(Items) -> Indice
    when Items :: [{Key, Value}] | [], Key :: term(), Value :: term(),
         Indice :: indice().
%% -----------------------------------------------------------------

from_orddict(Items) ->
    Keys = [K || {K, _} <- Items],
    {?MODULE, balance_list(Keys, length(Keys))}.


%% -----------------------------------------------------------------
%% @doc Creates an indice for a map using a custom comparison function.
%% @end
%% -----------------------------------------------------------------
-spec for_map(CompareFun, Map) -> indice()
    when CompareFun :: compare_fun(), Map :: erodlib:emap().
%% -----------------------------------------------------------------

for_map(CompareFun, Map) ->
    Fun = fun(Ka, Ka) -> true;
             (Ka, Kb) -> CompareFun(erodlib_maps:value(Ka, Map),
                                    erodlib_maps:value(Kb, Map))
          end,
    from_ordset(lists:usort(Fun, erodlib_maps:keys(Map))).


%% -----------------------------------------------------------------
%% @doc Gives the keys in indice order.
%% @end
%% -----------------------------------------------------------------
-spec keys(Indice) -> Result
    when Indice :: indice(), Result :: [term()] | [].
%% -----------------------------------------------------------------

keys({?MODULE, Tree}) ->
    tree_to_keys(Tree).


%% -----------------------------------------------------------------
%% @doc Gives the specified map's values in indice order.
%%
%% Assumes all the indice keys are present in the given map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec values(Map, Indice) -> Result
    when Map :: erodlib:emap(), Indice :: indice(), Result :: [term()] | [].
%% -----------------------------------------------------------------

values(Map, {?MODULE, Tree}) ->
    tree_to_values(Map, Tree).


%% -----------------------------------------------------------------
%% @doc Gives key-value pairs of specified map in the indice order.
%%
%% Assumes all the indice keys are present in the given map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec items(Map, Indice) -> Result
    when Map :: erodlib:emap(), Indice :: indice(),
         Result :: [{Key, Value}] | [], Key :: term(), Value :: term().
%% -----------------------------------------------------------------

items(Map, {?MODULE, Tree}) ->
    tree_to_list(Map, Tree).


%% -----------------------------------------------------------------
%% @doc Gives the list resulting of mapping a function on each elements
%% of specified map in indice order.
%%
%% Assumes all the indice keys are present in the given map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec map(MapFun, Map, Indice) -> Result
    when MapFun :: map_fun(), Map :: erodlib:emap(),
         Indice :: indice(), Result :: [term()] | [].
%% -----------------------------------------------------------------

map(Fun, Map, {?MODULE, Tree}) ->
    tree_map(Fun, Map, Tree).


%% -----------------------------------------------------------------
%% @doc Gives the numer of keys of the indice.
%% @end
%% -----------------------------------------------------------------
-spec size(Indice) -> Size
    when Indice :: indice(), Size :: non_neg_integer().
%% -----------------------------------------------------------------

size({?MODULE, Tree}) ->
    tree_size(Tree).


%% -----------------------------------------------------------------
%% @doc Gives the position of a key in the indice for a given map
%% and comparison function.
%%
%% Assumes that the key is present in the indice an map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec position(Key, CompareFun, Map, Indice) -> Position
    when Key :: term(), CompareFun :: compare_fun(), Map :: erodlib:emap(),
         Indice :: indice(), Position :: pos_integer().
%% -----------------------------------------------------------------

position(Key, CompareFun, Map, Indice) ->
    position(Key, erodlib_maps:value(Key, Map), CompareFun, Map, Indice).


%% -----------------------------------------------------------------
%% @doc Gives the position of a key in the indice for a given map
%% and comparison function.
%%
%% Assumes that the key is present in the indice an map, crashes otherwise.
%%
%% This function is just a shortcut for the {@link position/4} function
%% when the caller already knows the value associated with the key.
%% If the given value is NOT the value associated with the key in
%% the given map, the behavior of the function is undefined.
%% @end
%% -----------------------------------------------------------------
-spec position(Key, Value, CompareFun, Map, Indice) -> Position
    when Key :: term(), Value :: term(), CompareFun :: compare_fun(),
         Map :: erodlib:emap(), Indice :: indice(),
         Position :: pos_integer().
%% -----------------------------------------------------------------

position(Key, Value, CompareFun, Map, {?MODULE, Tree}) ->
    tree_position(Key, Value, CompareFun, Map, Tree).


%% -----------------------------------------------------------------
%% @doc Inserts a key in the indice using the comparison function
%% and the values in the given map to find out its position.
%% Returns the new indice and the position where the key has been inserted.
%%
%% Assumes that the key is not present in the indice and map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec insert(Key, CompareFun, Map, Indice) -> {Position, Indice}
    when Key :: term(), CompareFun :: compare_fun(),
         Map :: erodlib:emap(), Indice :: indice(),
         Position :: pos_integer().
%% -----------------------------------------------------------------

insert(Key, CompareFun, Map, Indice) ->
    insert(Key, erodlib_maps:value(Key, Map), CompareFun, Map, Indice).


%% -----------------------------------------------------------------
%% @doc Inserts a key in the indice using the comparison function
%% and the values in the given map to find out its position.
%% Returns the new indice and the position where the key has been inserted.
%%
%% Assumes that the key is not present in the indice and map, crashes otherwise.
%%
%% This function is just a shortcut for the {@link insert/4} function
%% when the caller already knows the value associated with the key.
%% If the given value is NOT the value associated with the key in
%% the given map, the behavior of the function is undefined.
%% @end
%% -----------------------------------------------------------------
-spec insert(Key, Value, CompareFun, Map, Indice) -> {Position, Indice}
    when Key :: term(), Value :: term(), CompareFun :: compare_fun(),
         Map :: erodlib:emap(), Indice :: indice(),
         Position :: pos_integer().
%% -----------------------------------------------------------------

insert(Key, Value, CompareFun, Map, {?MODULE, Tree}) ->
    Size = tree_size(Tree),
    WeightRef = ?pow(Size + 1),
    {_, Pos, NewTree} = tree_insert(Key, Value, CompareFun, Map,
                                    Tree, WeightRef),
    {Pos, {?MODULE, NewTree}}.


%% -----------------------------------------------------------------
%% @doc Removes the key from the indice for given map and comparison function.
%% Returns the new indice and the position from where the key has been deleted.
%%
%% Assumes that the key is present in the indice an map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec delete(Key, CompareFun, Map, Indice) -> {Position, Indice}
    when Key :: term(), CompareFun :: compare_fun(),
         Map :: erodlib:emap(), Indice :: indice(),
         Position :: pos_integer().
%% -----------------------------------------------------------------

delete(Key, CompareFun, Map, Indice) ->
    delete(Key, erodlib_maps:value(Key, Map), CompareFun, Map, Indice).


%% -----------------------------------------------------------------
%% @doc Removes the key from the indice for given map and comparison function.
%% Returns the new indice and the position from where the key has been deleted.
%%
%% Assumes that the key is present in the indice an map, crashes otherwise.
%%
%% This function is just a shortcut for the {@link delete/4} function
%% when the caller already knows the value associated with the key.
%% If the given value is NOT the value associated with the key in
%% the given map, the behavior of the function is undefined.
%% @end
%% -----------------------------------------------------------------
-spec delete(Key, Value, CompareFun, Map, Indice) -> {Position, Indice}
    when Key :: term(), Value :: term(), CompareFun :: compare_fun(),
         Map :: erodlib:emap(), Indice :: indice(),
         Position :: pos_integer().
%% -----------------------------------------------------------------

delete(Key, Value, CompareFun, Map, {?MODULE, Tree}) ->
    {Pos, NewTree} = tree_delete(Key, Value, CompareFun, Map, Tree),
    {Pos, {?MODULE, NewTree}}.


%% -----------------------------------------------------------------
%% @doc Gives the smallest indice key.
%%
%% Assumes the indice is not empty, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec smallest_key(Indice) -> Key
    when Indice :: indice(), Key :: term().
%% -----------------------------------------------------------------

smallest_key({?MODULE, Tree}) ->
    tree_smallest(Tree).


%% -----------------------------------------------------------------
%% @doc Gives the map value with the smallest indice key.
%%
%% Assumes the indice is not empty and the key is present in the given map,
%% crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec smallest_value(Map, Indice) -> Value
    when Map :: erodlib:emap(), Indice :: indice(), Value :: term().
%% -----------------------------------------------------------------

smallest_value(Map, {?MODULE, Tree}) ->
    erodlib_maps:value(tree_smallest(Tree), Map).


%% -----------------------------------------------------------------
%% @doc Removes and returns the smallest indice key.
%%
%% Assumes the indice is not empty, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec take_smallest_key(Indice) -> {Key, Indice}
    when Indice :: indice(), Key :: term().
%% -----------------------------------------------------------------

take_smallest_key({?MODULE, Tree}) ->
    {SmallestKey, NewTree} = tree_take_smallest(Tree),
    {SmallestKey, {?MODULE, NewTree}}.


%% -----------------------------------------------------------------
%% @doc Removes the smallest indice key and returns the map value with this key.
%%
%% Assumes the indice is not empty and the key is present in the given map,
%% crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec take_smallest_value(Map, Indice) -> {Value, Indice}
    when Map :: erodlib:emap(), Indice :: indice(), Value :: term().
%% -----------------------------------------------------------------

take_smallest_value(Map, {?MODULE, Tree}) ->
    {SmallestKey, NewTree} = tree_take_smallest(Tree),
    {erodlib_maps:value(SmallestKey, Map), {?MODULE, NewTree}}.


%% -----------------------------------------------------------------
%% @doc Removes and returns the N smallest indice keys.
%% If there is not enough keys, all keys are returned.
%% @end
%% -----------------------------------------------------------------
-spec take_smallest_keys(Count, Indice) -> {Keys, Indice}
    when Count :: pos_integer(), Indice :: indice(),
         Keys :: [Key] | [], Key :: term().
%% -----------------------------------------------------------------

take_smallest_keys(_Count, {?MODULE, Tree}) ->
    %TODO: Implement take_smallest_keys/2.
    {[], {?MODULE, Tree}}.


%% -----------------------------------------------------------------
%% @doc Gives the largest indice key.
%%
%% Assumes the indice is not empty, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec largest_key(Indice) -> Key
    when Indice :: indice(), Key :: term().
%% -----------------------------------------------------------------

largest_key({?MODULE, Tree}) ->
    tree_largest(Tree).


%% -----------------------------------------------------------------
%% @doc Gives the map value with the largest indice key.
%%
%% Assumes the indice is not empty and the key is present in the given map,
%% crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec largest_value(Map, Indice) -> Value
    when Map :: erodlib:emap(), Indice :: indice(), Value :: term().
%% -----------------------------------------------------------------

largest_value(Map, {?MODULE, Tree}) ->
    erodlib_maps:value(tree_largest(Tree), Map).


%% -----------------------------------------------------------------
%% @doc Removes and returns the largest indice key.
%%
%% Assumes the indice is not empty, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec take_largest_key(Indice) -> {Key, Indice}
    when Indice :: indice(), Key :: term().
%% -----------------------------------------------------------------

take_largest_key({?MODULE, Tree}) ->
    {LargestKey, NewTree} = tree_take_largest(Tree),
    {LargestKey, {?MODULE, NewTree}}.


%% -----------------------------------------------------------------
%% @doc Removes the largest indice key and returns the map value with this key.
%%
%% Assumes the indice is not empty and the key is present in the given map,
%% crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec take_largest_value(Map, Indice) -> {Value, Indice}
    when Map :: erodlib:emap(), Indice :: indice(), Value :: term().
%% -----------------------------------------------------------------

take_largest_value(Map, {?MODULE, Tree}) ->
    {LargestKey, NewTree} = tree_take_largest(Tree),
    {erodlib_maps:value(LargestKey, Map), {?MODULE, NewTree}}.


%% -----------------------------------------------------------------
%% @doc Removes and returns the N largest indice keys.
%% If there is not enough keys, all keys are returned.
%% @end
%% -----------------------------------------------------------------
-spec take_largest_keys(Count, Indice) -> {Keys, Indice}
    when Count :: pos_integer(), Indice :: indice(),
         Keys :: [Key] | [], Key :: term().
%% -----------------------------------------------------------------

take_largest_keys(_Count, {?MODULE, Tree}) ->
    %TODO: Implement take_largest_keys/2.
    {[], {?MODULE, Tree}}.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

tree_to_keys(Tree) -> tree_to_keys(Tree, []).


tree_to_keys({Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    tree_to_keys(Smaller, [Key |tree_to_keys(Larger, Acc)]);

tree_to_keys(nil, Acc) -> Acc.


tree_to_values(Map, Tree) -> tree_to_values(Map, Tree, []).


tree_to_values(Map, {Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    Value = erodlib_maps:value(Key, Map),
    tree_to_values(Map, Smaller, [Value |tree_to_values(Map, Larger, Acc)]);

tree_to_values(_Map, nil, Acc) -> Acc.


tree_to_list(Map, Tree) -> tree_to_list(Map, Tree, []).


tree_to_list(Map, {Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    Item = {Key, erodlib_maps:value(Key, Map)},
    tree_to_list(Map, Smaller, [Item |tree_to_list(Map, Larger, Acc)]);

tree_to_list(_Map, nil, Acc) -> Acc.


tree_map(Fun, Map, Tree) -> tree_map(Fun, Map, Tree, []).


tree_map(Fun, Map, {Key, _Size, _Weight, Smaller, Larger}, Acc) ->
    Result = Fun({Key, erodlib_maps:value(Key, Map)}),
    tree_map(Fun, Map, Smaller, [Result |tree_map(Fun, Map, Larger, Acc)]);

tree_map(_Fun, _Map, nil, Acc) -> Acc.


tree_size(nil) -> 0;

tree_size({_Key, Size, _Weight, _Smaller, _Larger}) -> Size.


tree_position(RefKey, _Val, _Comp, _Map, {RefKey, _S, _W, Smaller, _Larger}) ->
    tree_size(Smaller) + 1;

tree_position(Key, Val, Comp, Map, {RefKey, _S, _W, Smaller, Larger}) ->
    case Comp(Val, erodlib_maps:value(RefKey, Map)) of
        true ->
            tree_position(Key, Val, Comp, Map, Smaller);
        false ->
            tree_size(Smaller) + 1 + tree_position(Key, Val, Comp, Map, Larger)
    end.


tree_insert(RefKey, _Val, _Comp, _Map, {RefKey, _S, _W, _Sm, _Lg}, _WRef) ->
    erlang:error({key_already_exists, RefKey});

tree_insert(Key, Val, Comp, Map, {RefKey, _S, _W, Smaller, Larger}, WRef) ->
    case Comp(Val, erodlib_maps:value(RefKey, Map)) of
        true ->
            case tree_insert(Key, Val, Comp, Map, Smaller, ?div2(WRef)) of
                {ok, Pos, NewSmaller} ->
                    {ok, Pos, combinate(RefKey, NewSmaller, Larger)};
                {check, Pos, NewSmaller} ->
                    NewTree = combinate(RefKey, NewSmaller, Larger),
                    {_, Size, Weight, _, _} = NewTree,
                    NewWRef = ?pow(Size),
                    if Weight > NewWRef -> {ok, Pos, balance_tree(NewTree)};
                       true -> {check, Pos, NewTree}
                    end
            end;
        false ->
            case tree_insert(Key, Val, Comp, Map, Larger, ?div2(WRef)) of
                {ok, Pos, NewLarger} ->
                    NewPos = tree_size(Smaller) + Pos + 1,
                    {ok, NewPos, combinate(RefKey, Smaller, NewLarger)};
                {check, Pos, NewLarger} ->
                    NewPos = tree_size(Smaller) + Pos + 1,
                    NewTree = combinate(RefKey, Smaller, NewLarger),
                    {_, Size, Weight, _, _} = NewTree,
                    NewWRef = ?pow(Size),
                    if Weight > NewWRef -> {ok, NewPos, balance_tree(NewTree)};
                       true -> {check, NewPos, NewTree}
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
    case Comp(Val, erodlib_maps:value(RefKey, Map)) of
        true ->
            {Pos, NewSmaller} = tree_delete(Key, Val, Comp, Map, Smaller),
            {Pos, combinate(RefKey, NewSmaller, Larger)};
        false ->
            {Pos, NewBigger} = tree_delete(Key, Val, Comp, Map, Larger),
            NewPos = tree_size(Smaller) + Pos + 1,
            {NewPos, combinate(RefKey, Smaller, NewBigger)}
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

