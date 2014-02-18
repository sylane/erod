-module(erod_maps).

-export([new/0,
         from_orddict/1,
         keys/1,
         lookup/2,
         value/2,
         insert/3,
         update/3,
         delete/2]).


new() ->
    gb_trees:empty().


from_orddict(OrdDict) ->
    gb_trees:from_orddict(OrdDict).


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

