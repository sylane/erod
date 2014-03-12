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
%%% @private
%%% ==========================================================================

-module(erod_document_sup).

-author('Sebastien Merle').

-behaviour(supervisor).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Processcontrol functions
-export([start_link/0]).

%%% API functions
-export([start_child/3]).

%%% Behaviour supervisor callbacks
-export([init/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(PROCESS, ?MODULE).


%%% ==========================================================================
%%% Process Control Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts and links a document process supervisor.
%% @end
%% -----------------------------------------------------------------
-spec start_link() -> {ok, Pid} | {error, Reason}
    when Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start_link() ->
    supervisor:start_link({local, ?PROCESS}, ?MODULE, []).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts a supervised document process with specified
%% factory module and options.
%% @end
%% -----------------------------------------------------------------
-spec start_child(DocKey, FacMod, FacOpts) -> {ok, Pid} | {error, Reason}
    when DocKey :: erod:key(), FacMod :: module(), FacOpts :: term(),
         Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start_child(DocKey, FacMod, FacOpts) ->
    supervisor:start_child(?PROCESS, [DocKey, FacMod, FacOpts]).


%%% ==========================================================================
%%% Behaviour supervisor Callbacks
%%% ==========================================================================

init([]) ->
    lager:info("Starting document supervisor...", []),
    {ok, { {simple_one_for_one, 5, 10},
          [{erod_document_worker, {erod_document_worker, start_link, []},
            transient, 5000, worker, [erod_document_worker]}]} }.

