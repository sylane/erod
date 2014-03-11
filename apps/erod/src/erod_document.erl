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
%%% @doc TODO: Document module erod_document.
%%% @end
%%% ==========================================================================

-module(erod_document).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erod_document.hrl").
-include("erod_context.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([new/3,
         key/1,
         get_content/2,
         get_children/4,
         patch_content/2,
         patch_content/3,
         add_child/2]).

%%% Internal API functions
-export([reply/2,
         handle_message/2]).

%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(Doc, ?MODULE).


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?Doc, {key :: erod:key(),
               mod :: module(),
               sub :: term(),
               content :: term(),
               children :: emap(),
               verlog :: verlog(),
               views :: emap()}).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

%%% Imported types
-type emap() :: erodlib:emap().
-type verlog() :: erod_document_verlog:verlog().

-type document() :: #?Doc{}.
-export_type([document/0]).


%%% ==========================================================================
%%% Behaviour erod_document Specification
%%% ==========================================================================

-callback init(DocKey, Options)
    -> {error, Reason}
     | {ok, Content, Children, Views, DocState}
    when DocKey :: erod:key(), Options :: list(), Reason :: term(),
         Content :: term(), Children :: list(),
         Views :: erod:view_specs(), DocState :: term().

-callback export_child_key(IntKey)
    -> ExtKey
    when IntKey :: term(), ExtKey :: erod:key().

-callback import_child_key(ExtKey)
    -> IntKey
    when ExtKey :: erod:key(), IntKey :: term().


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Creates a new document with specified key, callback module and options.
%% @end
%% -----------------------------------------------------------------
-spec new(DocKey, Module, Options) -> Document | {error, Reason}
    when DocKey :: erod:key(), Module :: module(), Options :: list(),
         Document :: document(), Reason :: term().
%% -----------------------------------------------------------------

new(DocKey, Module, Options) ->
    case Module:init(DocKey, Options) of
        {error, _Reason} = Error -> Error;
        {ok, Content, Children, ViewSpecs, Sub} ->
            ChildrenMap = erodlib_maps:from_items(Children),
            Views = create_views(ViewSpecs, Children, ChildrenMap),
            ViewMap = erodlib_maps:from_items(Views),
            erod_registry:register_document(DocKey, self()),
            #?Doc{key = DocKey,
                  mod = Module,
                  sub = Sub,
                  content = Content,
                  children = ChildrenMap,
                  verlog = erod_document_verlog:new(),
                  views = ViewMap}
    end.


%% -----------------------------------------------------------------
%% @doc Gives the document's key.
%% @end
%% -----------------------------------------------------------------
-spec key(Document) -> DocKey
    when Document :: document(), DocKey :: erod:key().
%% -----------------------------------------------------------------

key(#?Doc{key = Key}) -> Key.


%% -----------------------------------------------------------------
%% @doc Gives the document's content.
%%
%% If the specified version is undefined or not found in the history
%% the returned content's type will be 'entity' and contains the full
%% content. If the specified version is found in the history,
%% the returned content's type will be 'patch' and contains a cumulative
%% patch to get to the last version from the specified version.
%% If the content did not change since the specified version
%% 'unchanged' is returned.
%% @end
%% -----------------------------------------------------------------
-spec get_content(FromVer, Document) -> unchanged | Content
    when FromVer :: erod:version(), Document :: document(),
         Content :: erod:content().
%% -----------------------------------------------------------------

get_content(FromVer, Doc) ->
    #?Doc{key = DocKey, content = Content, verlog = VerLog} = Doc,
    case erod_document_verlog:get_patch(FromVer, VerLog) of
        unchanged -> unchanged;
        none ->
            Ver = erod_document_verlog:version(VerLog),
            {ok, #erod_content{key = DocKey, ver = Ver,
                               type = entity, data = Content}};
        {patch, Ver, Patch} ->
            {ok, #erod_content{key = DocKey, ver = Ver,
                               type = patch, data = Patch}}
    end.


%% -----------------------------------------------------------------
%% @doc Gives a page of document's children for a specified view.
%%
%% If the specified version is undefined or not found in the history
%% the returned content's type will be 'entity' and contains the full
%% content. If the specified version is found in the history,
%% the returned content's type will be 'patch' and contains a cumulative
%% patch to get to the last version from the specified version.
%% If the content did not change since the specified version
%% 'unchanged' is returned.
%% @end
%% -----------------------------------------------------------------
-spec get_children(ViewId, PageId, FromVer, Document)
    -> unchanged | Content | {error, Reason}
    when ViewId :: erod:view_id(), PageId :: erod:page_id(),
         FromVer :: erod:version(), Document :: document(),
         Content :: erod:content(),
         Reason :: view_not_found | page_not_found | term().
%% -----------------------------------------------------------------

get_children(ViewId, PageId, FromVer, Doc) ->
    #?Doc{mod = Mod, children = Children, views = Views} = Doc,
    case erodlib_maps:lookup(ViewId, Views) of
        none -> {error, view_not_found};
        {value, View} ->
            Fun = fun ({K, V}) -> {Mod:export_child_key(K), V} end,
            case erod_document_view:get_content(PageId, FromVer, Fun,
                                                Children, View) of
                unchanged -> unchanged;
                {error, _Reason} = Error -> Error;
                {Type, Ver, Size, Total, Data} ->
                    {ok, #erod_page{type = Type, ver = Ver,
                                    view = ViewId, page = PageId,
                                    size = Size, total = Total, data = Data}}
            end
    end.


%% -----------------------------------------------------------------
%% @doc Patches the document's content.
%% @end
%% -----------------------------------------------------------------
%% TODO: Enable when implemented.
%% -spec patch_content(Patch, Document) -> {ok, Document} | {error, Reason}
%%     when Patch :: erod:patch(), Document :: document(), Reason :: term().
%% -----------------------------------------------------------------

patch_content(_Patch, #?Doc{verlog = VerLog} = Doc) ->
    {ok, erod_document_verlog:version(VerLog), Doc}.


%% -----------------------------------------------------------------
%% @doc Patches the document's content if at specified version.
%% @end
%% -----------------------------------------------------------------
%% TODO: Enable when implemented.
%% -spec patch_content(Version, Patch, Document)
%%     -> {ok, Document} | {error, Reason}
%%     when Version :: erod:version() | undefined, Patch :: erod:patch(),
%%          Document :: document(), Reason :: conflict | term().
%% -----------------------------------------------------------------

patch_content(undefined, Patch, Doc) ->
    patch_content(Patch, Doc);

patch_content(RefVer, Patch, #?Doc{verlog = VerLog} = Doc) ->
    case erod_document_verlog:version(VerLog) =:= RefVer of
        false -> {error, conflict};
        true -> patch_content(Patch, Doc)
    end.


%% -----------------------------------------------------------------
%% @doc Adds a child to the document.
%% @end
%% -----------------------------------------------------------------
%% TODO: Enable when implemented.
%% -spec add_child(Child, Document) -> {ok, Document} | {error, Reason}
%%     when Child :: term(), Document :: document(), Reason :: term().
%% -----------------------------------------------------------------

add_child(_Child, Doc) ->
    {ok, Doc}.


%%% ==========================================================================
%%% Internal API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Reply to a synchronous call made to a document.
%% @end
%% -----------------------------------------------------------------
-spec reply(From, Reply) -> ok
    when From :: {pid(), reference()},Reply :: term().
%% -----------------------------------------------------------------

reply({To, Tag}, Reply) ->
    try To ! {Tag, Reply} of _ -> ok catch _:_ -> ok end.


%% -----------------------------------------------------------------
%% @doc Delegates a message to the document. If the document do not know
%% what to do with the message it returns 'ignored'.
%% @end
%% @private
%% -----------------------------------------------------------------
-spec handle_message(Msg, Document) -> ignored | {ok, Document}
    when Msg :: term(), Document :: document().
%% -----------------------------------------------------------------

handle_message({'$doc_perform', Act, [K |_] = A, Ctx}, #?Doc{key = K} = Doc) ->
    {ok, perform_action(Act, A, Ctx, Doc)};

handle_message({'$doc_perform', _Action, _Args, Ctx}, _Doc) ->
    erod_context:failed(document_not_found, Ctx),
    ignored;

handle_message({'$doc_call', Key, From, Request}, #?Doc{key = Key} = Doc) ->
    case handle_call(Request, From, Doc) of
        {reply, Reply, NewDoc} ->
            reply(From, Reply),
            {ok, NewDoc}
    end;

handle_message({'$doc_call', _Key, _From, _Request}, _Doc) ->
    ignored;

handle_message(_Msg, _Doc) ->
    ignored.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

handle_call({get_content, FromVer, _Watcher}, _From, Doc) ->
    {reply, get_content(FromVer, Doc), Doc};

handle_call({get_children, ViewId, PageId, FromVer, _Watcher}, _From, Doc) ->
    {reply, get_children(ViewId, PageId, FromVer, Doc), Doc};

handle_call(Request, From, Doc) ->
    lager:warning("Document ~p received unexpected request from ~p: ~p",
                  [Doc#?Doc.key, From, Request]),
    {reply, {error, unexpected}, Doc}.


create_views(ViewSpecs, Children, Map) ->
    create_views(ViewSpecs, Children, Map, []).


create_views([], _Children, _Map, Acc) -> Acc;

create_views([{Id, PageSize, CompareFun} |Rem], Children, Map, Acc) ->
    View = erod_document_view:from_items(Children, PageSize, CompareFun, Map),
    create_views(Rem, Children, Map, [{Id, View} |Acc]).


%%% --------------------------------------------------------------------------
%%% Action Handling
%%% --------------------------------------------------------------------------

perform_action(get_content, [_, Ver, Subs |_], Ctx, Doc) ->
    case get_content(Ver, Doc) of
        unchanged ->
            erod_context:done(unchanged, Ctx),
            perform_subscription(Subs, Ctx, Doc);
        {ok, Content} ->
            erod_context:done(Content, Ctx),
            perform_subscription(Subs, Ctx, Doc)
    end;

perform_action(get_children, [_, Ver, ViewId, PageId, Subs |_], Ctx, Doc) ->
    case get_children(ViewId, PageId, Ver, Doc) of
        {error, Reason} ->
            erod_context:failed(Reason, Ctx),
            Doc;
        unchanged ->
            erod_context:done(unchanged, Ctx),
            perform_subscription(Subs, Ctx, Doc);
        {ok, Page} ->
            erod_context:done(Page, Ctx),
            perform_subscription(Subs, Ctx, Doc)
    end;

perform_action(patch_content, [_, Ver, Patch|_], Ctx, Doc) ->
    case patch_content(Ver, Patch, Doc) of
        {error, Reason} ->
            erod_context:failed(Reason, Ctx),
            Doc;
        {ok, NewVer, NewDoc} ->
            erod_context:done(NewVer, Ctx),
            NewDoc
    end;

perform_action(Action, Args, Ctx, #?Doc{key = Key} = Doc) ->
    erod_context:error("Document ~p doesn't know how to perform action ~p "
                       "with arguments ~p.", [Key, Action, Args], Ctx),
    erod_context:failed(unknown_action, Ctx),
    Doc.


perform_subscription(true, #?Ctx{sess = Sess}, #?Doc{key = Key} = Doc)
  when Sess =/= undefined ->
    erod_registry:add_watcher(Key, Sess),
    Doc;

perform_subscription(true, Ctx, #?Doc{key = Key} = Doc) ->
    erod_context:warning("Document ~p cannot subscribe undefined session "
                         "as a watcher.", [Key], Ctx),
    Doc;

perform_subscription(false, _Ctx, Doc) ->
    Doc.
