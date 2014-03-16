%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erodws.
%%%
%%% Erodws is free software: you can redistribute it and/or modify
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

-module(dummy_document).

-author('Sebastien Merle').

-behaviour(erod_document_factory).
-behaviour(erod_document).


%%% ==========================================================================
%%% Icludes
%%% ==========================================================================

-include("dummy_document.hrl").


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
-define(Content, dummy_document_content).
-define(Child, dummy_document_child).


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


knows_content({dummy, _GroupId}, _Fac) -> true;

knows_content(_DocKey, _Fac) -> false.


get_content(DocKey, _Fac) ->
	get_content(DocKey).


start_document({dummy, Id} = DocKey, _Fac)
  when Id =:= 1; Id =:= 2 ->
    erod:start_document(DocKey, ?MODULE, []);

start_document(_DocKey, _Fac) ->
    {error, not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


%%% ==========================================================================
%%% Behaviour erod_document Callbacks
%%% ==========================================================================

init(DocKey, []) ->
    Views = [{asc, 3, fun compare_asc/2},
             {desc, 3, fun compare_desc/2},
             {asc_high_first, 3, fun compare_asc_high_first/2},
             {desc_high_first, 3, fun compare_desc_high_first/2}],
    case get_content(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, Content} ->
            case get_children(DocKey) of
                {error, _Reason} = Error -> Error;
                {ok, Children} -> {ok, Content, Children, Views, #?St{}}
            end
    end.


export_child_key(Key) -> {item, Key}.


import_child_key({item, Key}) -> Key.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

compare_asc(#?Child{desc = A}, #?Child{desc = B}) -> A =< B.


compare_desc(#?Child{desc = A}, #?Child{desc = B}) -> A > B.


compare_asc_high_first(#?Child{desc = A, pri = low},
                       #?Child{desc = B, pri = low}) -> A =< B;

compare_asc_high_first(#?Child{}, #?Child{pri = low}) -> true;

compare_asc_high_first(#?Child{pri = low}, #?Child{}) -> false;

compare_asc_high_first(#?Child{desc = A}, #?Child{desc = B}) -> A =< B.


compare_desc_high_first(#?Child{desc = A, pri = low},
                        #?Child{desc = B, pri = low}) -> A > B;

compare_desc_high_first(#?Child{}, #?Child{pri = low}) -> true;

compare_desc_high_first(#?Child{pri = low}, #?Child{}) -> false;

compare_desc_high_first(#?Child{desc = A}, #?Child{desc = B}) -> A > B.


get_content({dummy, 1}) ->
	{ok, #?Content{id = 1,
				   name = <<"foo">>,
				   label = "Some foo",
				   flag = false}};

get_content({dummy, 2}) ->
	{ok, #?Content{id = 2,
				   name = <<"bar">>,
				   label = "Some bar",
				   flag = true}};

get_content(_) ->
	{error, document_not_found}.



get_children({dummy, 1}) ->
	{ok, [{1, #?Child{pri = low, desc = "Archaeology Today"}},
		  {2, #?Child{pri = high, desc = "Blood, Devastation"}},
		  {3, #?Child{pri = low, desc = "Déjà Vu"}},
		  {4, #?Child{pri = high, desc = "Full Frontal Nudity"}},
		  {5, #?Child{pri = low, desc = "Grandstand"}},
		  {6, #?Child{pri = high, desc = "How Not to Be Seen"}},
		  {7, #?Child{pri = low, desc = "It's a Living"}}]};

get_children({dummy, 2}) ->
	{ok, [{1, #?Child{pri = low, desc = "Salad Days"}},
		  {2, #?Child{pri = high, desc = "Spam"}}]};

get_children(_) ->
	{error, document_not_found}.
