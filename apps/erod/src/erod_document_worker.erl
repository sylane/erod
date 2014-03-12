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
%%% @doc TODO: Document module erod_document_worker.
%%% @end
%%% @private
%%% ==========================================================================

-module(erod_document_worker).

-author('Sebastien Merle').

-behaviour(gen_server).


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Process control function
-export([start_link/3]).

%%% Behaviour gen_erver callacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {doc :: erod:document()}).


%%% ==========================================================================
%%% Proces Control Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts and links a new document process with specified
%% factory module and options.
%% @end
%% -----------------------------------------------------------------
-spec start_link(DocKey, FacMod, FacOpts) -> {ok, Pid} | {error, Reason}
    when DocKey :: erod:key(), FacMod :: module, FacOpts :: term(),
         Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start_link(DocKey, FacMod, FacOpts) ->
    gen_server:start_link(?MODULE, [DocKey, FacMod, FacOpts], []).


%%% ==========================================================================
%%% Behaviour gen_server Callbacks
%%% ==========================================================================

init([DocKey, FacMod, FacOpts]) ->
    lager:info("Worker process for document ~p started.", [DocKey]),
    case erod_document_factory:create_document(DocKey, FacMod, FacOpts) of
        {error, Reason} -> {stop, Reason};
        {ok, Doc} -> {ok, #?St{doc = Doc}}
    end.


handle_call(Request, {From, _Ref}, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info(Info, #?St{doc = Doc} = State) ->
    case erod_document:handle_message(Info, Doc) of
        {ok, NewDoc} -> {noreply, State#?St{doc = NewDoc}};
        ignored ->
            lager:warning("Unexpected message: ~p", [Info]),
            {noreply, State}
    end.


terminate(Reason, #?St{doc = Doc}) ->
    lager:info("Worker process for document ~p has been terminated: ~p",
               [erod_document:key(Doc), Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
