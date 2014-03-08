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
%%% @doc TODO: Document module erodlib_sets.
%%% @end
%%% ==========================================================================

-module(erodlib_sets).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/0,
         values/1,
         add/2]).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type eset() :: gb_set().
-export_type([eset/0]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new empty set.
%% @end
%% -----------------------------------------------------------------
-spec new() -> Set
    when Set :: eset().
%% -----------------------------------------------------------------

new() ->
    gb_sets:new().


%% -----------------------------------------------------------------
%% @doc Gives the ordered list of values from the set.
%% @end
%% -----------------------------------------------------------------
-spec values(Set) -> Values
    when Set :: eset(), Values :: [Value] | [], Value :: term().
%% -----------------------------------------------------------------

values(Set) ->
    gb_sets:to_list(Set).


%% -----------------------------------------------------------------
%% @doc Adds a value to the set.
%% If the value is already present in the set, nothing is changed.
%% @end
%% -----------------------------------------------------------------
-spec add(Value, Set) -> Set
    when Set :: eset(), Value :: term().
%% -----------------------------------------------------------------

add(Element, Set) ->
    gb_sets:add(Element, Set).
