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
%%% @doc TODO: Document module erodlib_maps.
%%% @end
%%% ==========================================================================

-module(erodlib_maps).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
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


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type emap() :: gb_tree().
-export_type([emap/0]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new empty map.
%% @end
%% -----------------------------------------------------------------
-spec new() -> Map
    when Map :: emap().
%% -----------------------------------------------------------------

new() ->
    gb_trees:empty().


%% -----------------------------------------------------------------
%% @doc Creates a new ap from an ordered list of items.
%% @end
%% -----------------------------------------------------------------
-spec from_orddict(Items) -> Map
    when Items :: [{Key, Value}] | [], Key :: term(), Value :: term(),
         Map :: emap().
%% -----------------------------------------------------------------

from_orddict(OrdDict) ->
    gb_trees:from_orddict(OrdDict).


%% -----------------------------------------------------------------
%% @doc Creates a new map from an unordered list of items.
%% @end
%% -----------------------------------------------------------------
-spec from_items(Items) -> Map
    when Items :: [{Key, Value}] | [], Key :: term(), Value :: term(),
         Map :: emap().
%% -----------------------------------------------------------------

from_items(Items) ->
    gb_trees:from_orddict(orddict:from_list(Items)).


%% -----------------------------------------------------------------
%% @doc Gives the number of nodes in the map.
%% @end
%% -----------------------------------------------------------------
-spec size(Map) -> Size
    when Map :: emap(), Size :: pos_integer().
%% -----------------------------------------------------------------

size(Map) ->
    gb_trees:size(Map).


%% -----------------------------------------------------------------
%% @doc Gives the ordered list of keys of a map.
%% @end
%% -----------------------------------------------------------------
-spec keys(Map) -> Keys
    when Map :: emap(), Keys :: [Key] | [], Key :: term().
%% -----------------------------------------------------------------

keys(Map) ->
    gb_trees:keys(Map).


%% -----------------------------------------------------------------
%% @doc Gives the map value for given key or 'none'.
%% @end
%% -----------------------------------------------------------------
-spec lookup(Key, Map) -> {value, Value} | none
    when Key :: term(), Map :: emap(), Value :: term().
%% -----------------------------------------------------------------

lookup(Key, Map) ->
    gb_trees:lookup(Key, Map).


%% -----------------------------------------------------------------
%% @doc Gives the map value for given key assuming it exists, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec value(Key, Map) -> Value
    when Key :: term(), Map :: emap(), Value :: term().
%% -----------------------------------------------------------------

value(Key, Map) ->
    {value, Value} = gb_trees:lookup(Key, Map),
    Value.


%% -----------------------------------------------------------------
%% @doc Inserts a node with specified key and value in the map.
%% Assume the key is not present in the map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec insert(Key, Value, Map) -> Map
    when Key :: term(), Value :: term(), Map :: emap().
%% -----------------------------------------------------------------

insert(Key, Value, Map) ->
    gb_trees:insert(Key, Value, Map).


%% -----------------------------------------------------------------
%% @doc Update the map value with specified key.
%% Assume the key is present in the map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec update(Key, Value, Map) -> Map
    when Key :: term(), Value :: term(), Map :: emap().
%% -----------------------------------------------------------------

update(Key, Value, Map) ->
    gb_trees:update(Key, Value, Map).


%% -----------------------------------------------------------------
%% @doc Removes the node with specified key from the map.
%% Assume the key is present in the map, crashes otherwise.
%% @end
%% -----------------------------------------------------------------
-spec delete(Key, Map) -> Map
    when Key :: term(), Map :: emap().
%% -----------------------------------------------------------------

delete(Key, Map) ->
    gb_trees:delete(Key, Map).

%% -----------------------------------------------------------------
%% @doc Lookup all the values starting from the specified key.
%% If the specified key is not found 'none' is returned.
%% @end
%% -----------------------------------------------------------------
-spec lookup_from(Key, Map) -> {values, Values} | none
    when Key :: term(), Map :: emap(), Values :: [Value], Value :: term().
%% -----------------------------------------------------------------

lookup_from(Key, {_Size, Tree}) ->
    case tree_lookup_from(Key, Tree) of
        none -> none;
        Values -> {values, lists:reverse(Values)}
    end.


%% -----------------------------------------------------------------
%% @doc Removes the smallest nodes so the map size become the specified size.
%% @end
%% -----------------------------------------------------------------
-spec trim_smallest(Size, Map) -> Map
    when Size :: pos_integer() | 0, Map :: emap().
%% -----------------------------------------------------------------

trim_smallest(MinSize, {Size, Tree}) when Size > MinSize ->
    %TODO: Implement trim_smallest/2.
    {Size, Tree};

trim_smallest(_MinSize, Map) -> Map.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

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
