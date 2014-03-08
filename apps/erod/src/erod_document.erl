-module(erod_document).

-include("erod_document.hrl").
-include("erod_context.hrl").

-export([new/3,
         key/1,
         get_content/2,
         get_children/4,
         patch_content/2,
         patch_content/3,
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

-type document() :: #?Doc{}.
-export_type([document/0]).


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


-spec new(DocKey :: erod:key(), Module :: module(), Options :: list()) ->
          erod:document() | {error, Reason :: term()}.

new(DocKey, Module, Options) ->
    case Module:init(DocKey, Options) of
        {error, _Reason} = Error -> Error;
        {ok, Content, Children, ViewSpecs, State} ->
            ChildrenMap = erodlib_maps:from_items(Children),
            Views = create_views(ViewSpecs, Children, ChildrenMap),
            ViewMap = erodlib_maps:from_items(Views),
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


get_children(ViewId, PageId, FromVer, Doc) ->
    #?Doc{sub_mod = Mod, children = Children, views = Views} = Doc,
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


patch_content(_Patch, #?Doc{verlog = VerLog} = Doc) ->
    {ok, erod_document_verlog:version(VerLog), Doc}.


patch_content(undefined, Patch, Doc) ->
    patch_content(Patch, Doc);

patch_content(RefVer, Patch, #?Doc{verlog = VerLog} = Doc) ->
    case erod_document_verlog:version(VerLog) =:= RefVer of
        false -> {error, conflict};
        true -> patch_content(Patch, Doc)
    end.


add_child(_Child, Doc) ->
    {ok, Doc}.


patch_child(_ChildKey, _Patch, Doc) ->
    {ok, Doc}.


handle_message({'$doc_perform', Act, [K |_] = A, Ctx}, #?Doc{key = K} = Doc) ->
    {ok, perform_action(Act, A, Ctx, Doc)};

handle_message({'$doc_perform', _Action, _Args, _Ctx}, _Doc) ->
    erod_context:debug("Ignore ~p ~p: ~p", [_Action, _Args, _Doc#?Doc.key], _Ctx),
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
