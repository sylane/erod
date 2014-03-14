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

-module(erdom_sup).

-author('Sebastien Merle').

-behaviour(supervisor).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control functions
-export([start_link/0]).

%%% Behaviour supervisor callbacks
-export([init/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%%% ==========================================================================
%%% Process Control Functions
%%% ==========================================================================

start_link() ->
    supervisor:start_link(?MODULE, []).



%%% ==========================================================================
%%% Behaviour supervisor Callbacks
%%% ==========================================================================

init([]) ->
    lager:debug("Starting root supervisor...", []),
    {ok, {{one_for_one, 5, 10}, [?CHILD(erdom_storage, worker)]}}.
