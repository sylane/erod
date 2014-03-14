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
%%% @doc TODO: Document module erod_actions.
%%% @end
%%% ==========================================================================

-module(erdom_document_group).

-author('Sebastien Merle').

-behaviour(erod_document_factory).
-behaviour(erod_document).


%%% ==========================================================================
%%% Icludes
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
-define(UserContent, erdom_document_user_content).
-define(Content, erdom_document_group_content).
-define(Child, erdom_document_group_child).


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


knows_content({group, _GroupId}, _Fac) -> true;

knows_content(_DocKey, _Fac) -> false.


get_content(DocKey, _Fac) -> get_content(DocKey).


start_document({group, GroupId} = DocKey, _Fac) ->
    case erdom_storage:does_group_exist(GroupId) of
        true -> erod:start_document(DocKey, ?MODULE, []);
        false -> {error, document_not_found}
    end;

start_document(_DocKey, _Fac) ->
    {error, not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


%%% ==========================================================================
%%% Behaviour erod_document Callbacks
%%% ==========================================================================

init(DocKey, []) ->
    Views = [{asc, 50, fun compare_asc/2},
             {desc, 50, fun compare_desc/2},
             {asc_pres_first, 50, fun compare_asc_pres_first/2},
             {desc_pres_first, 50, fun compare_desc_pres_first/2}],
    case get_content(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, Content} ->
            case get_children(DocKey) of
                {error, _Reason} = Error -> Error;
                {ok, Children} -> {ok, Content, Children, Views, #?St{}}
            end
    end.


export_child_key(Key) -> {user, Key}.


import_child_key({user, Key}) -> Key.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

compare_asc(#?Child{name = A}, #?Child{name = B}) -> A =< B.


compare_desc(#?Child{name = A}, #?Child{name = B}) -> A > B.


compare_asc_pres_first(#?Child{name = A, presence = offline},
                       #?Child{name = B, presence = offline}) -> A =< B;

compare_asc_pres_first(#?Child{}, #?Child{presence = offline}) -> true;

compare_asc_pres_first(#?Child{presence = offline}, #?Child{}) -> false;

compare_asc_pres_first(#?Child{name = A}, #?Child{name = B}) -> A =< B.


compare_desc_pres_first(#?Child{name = A, presence = offline},
                        #?Child{name = B, presence = offline}) -> A > B;

compare_desc_pres_first(#?Child{}, #?Child{presence = offline}) -> true;

compare_desc_pres_first(#?Child{presence = offline}, #?Child{}) -> false;

compare_desc_pres_first(#?Child{name = A}, #?Child{name = B}) -> A > B.



get_content({group, GroupId}) ->
    case erdom_storage:get_group_content(GroupId) of
        {error, group_not_found} -> {error, document_not_found};
        {error, _Reason} = Error -> Error;
        {ok, GroupData} ->
            #erdom_storage_group{name = N} = GroupData,
            {ok, #?Content{name = N}}
    end.


get_children({group, GroupId}) ->
    case erdom_storage:get_group_children(GroupId) of
        {error, _Reason} = Error -> Error;
        {ok, UserIds} ->
            {ok, [{UID, C} || UID <- UserIds,
                  begin {F, C} = get_child(UID), F end]}
    end.


get_child(UserId) ->
    case erod_registry:get_content({user, UserId}, undefined, self()) of
        {ok, #erod_content{type = entity, data = UC}} ->
            #?UserContent{display_name = N, presence = P, connected = C} = UC,
            {true, #?Child{name = N, presence = P, connected = C}};
        _ -> {false, undefined}
    end.
