%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erdom.
%%%
%%% Erdom is free software: you can redistribute it and/or modify
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
%%% @doc TODO: Document module erdom_document_index.
%%% @end
%%% ==========================================================================

-module(erdom_document_index).

-author('Sebastien Merle').

-behaviour(erod_document_factory).
-behaviour(erod_document).

%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erdom_document.hrl").
-include("erdom_storage.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Behaviour erod_document_factory callbacks
-export([init_factory/1,
         knows_content/2,
         get_content/2,
         start_document/2,
         create_document/2]).

%%% Behaviour erod_document callbacks
-export([init/2,
         export_child_key/1,
         import_child_key/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).
-define(GroupContent, erdom_document_group_content).
-define(Content, erdom_document_index_content).
-define(Child, erdom_document_index_child).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {}).
-record(fac, {}).


%%% ==========================================================================
%%% Behaviour erod_document_factory Callbacks
%%% ==========================================================================

init_factory([]) ->
    {ok, #fac{}}.


knows_content({index, _IndexId}, _Fac) -> true;

knows_content(_DocKey, _Fac) -> false.


get_content({index, 0}, _Fac) ->
    {ok, #?Content{}};

get_content({index, _IndeId}, _Fac) ->
    {error, document_not_found}.


start_document({index, 0} = DocKey, _Fac) ->
    erod:start_document(DocKey, ?MODULE, []);

start_document(_DocKey, _Fac) ->
    {error, document_not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


%%% ==========================================================================
%%% Behaviour erod_document Callbacks
%%% ==========================================================================

init({index, 0}, []) ->
    Views = [{asc, 50, fun compare_asc/2},
             {desc, 50, fun compare_desc/2}],
    Content = #?Content{},
    case get_children() of
        {error, _Reason} = Error -> Error;
        {ok, Children} -> {ok, Content, Children, Views, #?St{}}
    end.


export_child_key(Key) -> {group, Key}.


import_child_key({group, Key}) -> Key.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

compare_asc(#?Child{name = A}, #?Child{name = B}) -> A =< B.


compare_desc(#?Child{name = A}, #?Child{name = B}) -> A > B.


get_children() ->
    case erdom_storage:get_index_children() of
        {error, _Reason} = Error -> Error;
        {ok, GroupIds} ->
            {ok, [{GID, C} || GID <- GroupIds,
                  begin {F, C} = get_child(GID), F end]}
    end.


get_child(GroupId) ->
    case erod_registry:get_content({group, GroupId}, undefined, self()) of
        {ok, #erod_content{type = entity, data = GroupContent}} ->
            #?GroupContent{name = Name} = GroupContent,
            {true, #?Child{name = Name}};
        _ -> {false, undefined}
    end.
