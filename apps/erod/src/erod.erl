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
%%% @doc TODO: Document module erod.
%%% @end
%%% ==========================================================================

-module(erod).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_document.hrl").
-include("erod_policy.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([start_document/3]).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type key() :: erodlib:erod_key().
-type version() :: erodlib:erod_version().
-type patch_path_part() :: erodlib:erod_patch_path_part().
-type patch_entry() :: erodlib:erod_patch_entry().
-type patch() :: erodlib:erod_patch().

-type view_id() :: atom().
-type page_id() :: pos_integer().
-type user_id() :: pos_integer().
-type session_id() :: pos_integer().
-type session_token() :: binary().

-type content_type() :: entity | patch.
-type entity() :: tuple().
-type entity_item() :: {key(), entity()}.
-type entity_items() :: list(entity_item()) | [].

-type view_spec() :: {ViewId :: atom(),
                      PageSize :: pos_integer(),
                      OrderFun :: function()}.
-type view_specs() :: list(view_spec()) | [].

-type content() :: #erod_content{}.
-type page() :: #erod_page{}.
-type policy() :: #erod_policy{}.

-type action_id() :: atom().
-type action_args() :: [term()] | [].
-type action() :: {action_id(), action_args()}.
-type actions() :: [action()] | [].

-type document() :: erod_document:document().
-type context() :: erod_context:context().
-type proxy() :: erod_proxy:proxy().

-export_type([key/0, version/0, view_id/0, page_id/0,
              user_id/0, session_id/0, session_token/0,
              content_type/0, view_spec/0, view_specs/0,
              entity/0, entity_item/0, entity_items/0,
              patch_path_part/0, patch_entry/0, patch/0,
              content/0, page/0, policy/0,
              action_id/0, action_args/0, action/0, actions/0,
              document/0, context/0, proxy/0]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Starts a document process for the gien document factory and options.
%% @end
%% -----------------------------------------------------------------
-spec start_document(DocKey, FactMod, Opts) -> {ok, Pid} | {error, Reason}
    when DocKey :: erod:key(), FactMod :: module(), Opts :: term(),
         Pid :: pid(), Reason :: term().
%% -----------------------------------------------------------------

start_document(DocKey, FactMod, Opts) ->
    erod_document_sup:start_child(DocKey, FactMod, Opts).
