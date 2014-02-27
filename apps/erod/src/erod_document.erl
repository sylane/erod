-module(erod_document).

-include("erod_internal.hrl").

-export([new/3,
         key/1,
         get_content/2,
         get_children/4,
         patch_content/2,
         add_child/2,
         patch_child/3]).

-export([handle_message/2]).

-define(Doc, ?MODULE).
-record(?Doc, {key,
               sub_mod,
               sub_state,
               content,
               children,
               verlog,
               views}).

%%%TODO: Add version match parameter (for implementing caching)

-callback init(DocKey :: erod:key(), Options :: list()) ->
    {ok, Content :: term(), Children :: list(),
     Views :: erod:view_specs(), DocState :: term()}.


-callback export_child_key(IntKey :: term()) -> ExtKey :: erod:key().


-callback import_child_key(ExtKey :: erod:key()) -> IntKey :: term().


-spec new(DocKey :: erod:key(), Module :: module(), Options :: list()) ->
          erod:document().

new(DocKey, Module, Options) ->
    case Module:init(DocKey, Options) of
        {ok, Content, Children, ViewSpecs, State} ->
            ChildrenMap = erod_maps:from_items(Children),
            Views = create_views(ViewSpecs, Children, ChildrenMap),
            ViewMap = erod_maps:from_items(Views),
            erod_registry:register_document(DocKey, self()),
            #?Doc{key = DocKey,
                  sub_mod = Module,
                  sub_state = State,
                  content = Content,
                  children = ChildrenMap,
                  verlog = erod_document_verlog:new(),
                  views = ViewMap}
    end.


key(#?Doc{key = Key}) -> Key.


get_content(FromVer, Doc) ->
    #?Doc{key = DocKey, content = Content, verlog = VerLog} = Doc,
    {Type, Ver, Data} =
        case erod_document_verlog:get_patch(FromVer, VerLog) of
            none -> {entity, erod_document_verlog:version(VerLog), Content};
            unchanged -> unchanged;
            Patch -> Patch
        end,
    #erod_content{key = DocKey, ver = Ver, type = Type, data = Data}.


get_children(ViewId, PageId, FromVer, Doc) ->
    #?Doc{sub_mod = Mod, children = Children, views = Views} = Doc,
    case erod_maps:lookup(ViewId, Views) of
        none -> {error, view_not_found};
        {value, View} ->
            Fun = fun ({K, V}) -> {Mod:export_child_key(K), V} end,
            case erod_document_view:get_content(PageId, FromVer, Fun,
                                                Children, View) of
                unchanged -> unchanged;
                {error, _Reason} = Error -> Error;
                {Type, Ver, Size, Total, Data} ->
                    #erod_page{type = Type, ver = Ver,
                               view = ViewId, page = PageId,
                               size = Size, total = Total, data = Data}
            end
    end.


patch_content(_Patch, Doc) ->
    {ok, Doc}.


add_child(_Child, Doc) ->
    {ok, Doc}.


patch_child(_ChildKey, _Patch, Doc) ->
    {ok, Doc}.


handle_message({'$doc_call', Key, From, Request}, #?Doc{key = Key} = Doc) ->
    case handle_call(Request, From, Doc) of
        {noreply, NewDoc} -> {ok, NewDoc};
        {reply, Reply, NewDoc} ->
            reply(From, Reply),
            {ok, NewDoc}
    end;

handle_message({'$doc_call', _Key, _From, _Request}, _Doc) ->
    ignored;

handle_message(_Msg, _Doc) ->
    ignored.


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


reply({To, Tag}, Reply) ->
    try To ! {Tag, Reply} of _ -> ok catch _:_ -> ok end.
