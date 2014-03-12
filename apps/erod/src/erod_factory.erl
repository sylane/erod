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
%%% @doc TODO: Document module erod_factory.
%%% @end
%%% ==========================================================================

-module(erod_factory).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_document.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
 -export([new/2,
          knows_content/2,
          get_content/2,
          start_document/2,
          create_document/3]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(Fac, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?Fac, {mod, sub}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type factory() :: #?Fac{}.
-export_type([factory/0]).


%%% ==========================================================================
%%% Behaviour erod_factory Specification
%%% ==========================================================================

-callback init_factory(Options)
    -> {ok, State}
    when Options :: term(), State :: term().

-callback knows_content(DocKey, State)
    -> boolean()
    when DocKey :: erod:key(), State :: term().

-callback get_content(DocKey, State)
    -> {error, Reason}
     | {ok, Content}
    when DocKey :: erod:key(), State :: term(),
         Reason :: term(), Content :: term().

-callback start_document(DocKey, State)
    -> {ok, DocPid}
     | {error, Reason}
    when DocKey :: erod:key(), State :: term(),
         DocPid :: pid(), Reason :: term().

-callback create_document(DocKey, Options)
    -> {ok, Doc}
    when DocKey :: erod:key(), Options :: list(), Doc :: erod:document().


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new factory with specified callback module and options.
%% @end
%% -----------------------------------------------------------------
-spec new(Module, Options) -> {ok, Factory}
    when Module :: module(), Options :: term(), Factory :: factory().
%% -----------------------------------------------------------------

new(Module, Options) ->
    {ok, Sub} = Module:init_factory(Options),
    {ok, #?Fac{mod = Module, sub = Sub}}.


%% -----------------------------------------------------------------
%% @doc Tells if the factory knows how to give the document content
%% even if the document has not been started yet.
%% If this function return true, {@link get_content/2} should return
%% the document content.
%% @end
%% -----------------------------------------------------------------
-spec knows_content(DocKey, Factory) -> boolean()
    when DocKey :: erod:key(), Factory :: factory().
%% -----------------------------------------------------------------

knows_content(DocKey, #?Fac{mod = Mod, sub = Sub}) ->
    Mod:knows_content(DocKey, Sub).


%% -----------------------------------------------------------------
%% @doc Gives the content of a document directly from storage.
%% Used when other object retrieve the content and there is no
%% process for the requested documnt.
%% This prevent having to start and load the whole hierarchy of documents
%% when starting the root document.
%% @end
%% -----------------------------------------------------------------
-spec get_content(DocKey, Factory) -> {ok, Content} | {error, Reason}
    when DocKey :: erod:key(), Factory :: factory(), Content :: erod:content(),
         Reason :: document_not_found | term().
%% -----------------------------------------------------------------

get_content(DocKey, #?Fac{mod = Mod, sub = Sub}) ->
    case Mod:get_content(DocKey, Sub) of
        {error, _Reason} = Error -> Error;
        {ok, Content} ->
            {ok, #erod_content{key = DocKey, type = entity, data = Content}}
    end.


%% -----------------------------------------------------------------
%% @doc Starts a process for given document.
%% @end
%% -----------------------------------------------------------------
-spec start_document(DocKey, Factory) -> {ok, Pid} | {error, Reason}
    when DocKey :: erod:key(), Factory :: factory(),
         Pid :: pid(), Reason :: document_not_found | term().
%% -----------------------------------------------------------------

start_document(DocKey, #?Fac{mod = Mod, sub = Sub}) ->
    Mod:start_document(DocKey, Sub).


%% -----------------------------------------------------------------
%% @doc Creates a document with given callback module and options.
%% @end
%% -----------------------------------------------------------------
-spec create_document(DocKey, Module, Options)
    -> {ok, Document} | {error, Reason}
    when DocKey :: erod:key(), Module :: module(), Options :: term(),
         Document :: erod:document(), Reason :: term().
%% -----------------------------------------------------------------

create_document(DocKey, Module, Options) ->
    Module:create_document(DocKey, Options).
