-module(erod_sets).

-export([new/0,
         to_list/1,
         add/2]).


new() ->
    gb_sets:new().


to_list(Set) ->
    gb_sets:to_list(Set).


add(Element, Set) ->
    gb_sets:add(Element, Set).
