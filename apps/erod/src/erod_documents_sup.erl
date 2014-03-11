%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erod.
%%%
%%% Erod is free software: you can redistribute it and/or modify
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
%%% @doc TODO: Document module erod_document_sup.
%%% @end
%%% ==========================================================================

-module(erod_documents_sup).

-author('Sebastien Merle').

-behaviour(supervisor).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control functions
-export([start_link/0]).

%%% behaviour supervisor callbacks
-export([init/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).


%%% ==========================================================================
%%% Process Control Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts and links the main supervisor for documents and registry.
%% @end
%% -----------------------------------------------------------------
-spec start_link() -> {ok, Pid} | {error, Reason}
    when Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start_link() ->
    supervisor:start_link(?MODULE, []).


%%% ==========================================================================
%%% Behaiour supervisor Callbacks
%%% ==========================================================================

init([]) ->
    lager:info("Starting documents supervisor...", []),
    {ok, { {rest_for_one, 5, 10},
           [?CHILD(erod_registry, worker),
            ?CHILD(erod_document_sup, supervisor)]} }.

