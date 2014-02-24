-module(erod_document).

-export([new/3,
         key/1,
         get_content/2,
         get_children/4,
         patch_content/2,
         add_child/2,
         patch_child/3]).

-export([handle_message/2]).

-export(['_get_content'/2,
         '_get_and_watch'/3,
         '_register_interest'/3,
         '_unregister_interest'/3,
         '_notify_change'/3,
         '_notify_state'/3]).

-define(Doc, ?MODULE).
-record(?Doc, {key,
               sub_mod,
               sub_state,
               content,
               children,
               verlog,
               views}).


new(DocKey, Module, Options) ->
    case Module:init(DocKey, Options, #?Doc{key = DocKey}) of
        {ok, Content, Children, ViewSpecs, State, Doc} ->
            ChildrenMap = erod_maps:from_items(Children),
            Views = create_views(ViewSpecs, Children, ChildrenMap),
            ViewMap = erod_maps:from_items(Views),
            erod_registry:register_document(DocKey, self()),
            Doc#?Doc{sub_mod = Module,
                     sub_state = State,
                     content = Content,
                     children = ChildrenMap,
                     verlog = erod_document_verlog:new(),
                     views = ViewMap}
    end.


key(#?Doc{key = Key}) -> Key.


get_content(FromVer, #?Doc{content = Content, verlog = VerLog}) ->
    case erod_document_verlog:get_patch(FromVer, VerLog) of
        none -> {content, erod_document_verlog:version(VerLog), Content};
        Patch -> Patch
    end.


get_children(ViewId, PageId, FromVer, Doc) ->
    #?Doc{children = Children, views = Views} = Doc,
    case erod_maps:lookup(ViewId, Views) of
        none -> view_not_found;
        {value, View} ->
            erod_document_view:get_content(PageId, FromVer, Children, View)
    end.


patch_content(_Patch, Doc) ->
    {ok, Doc}.


add_child(_Child, Doc) ->
    {ok, Doc}.


patch_child(_ChildKey, _Patch, Doc) ->
    {ok, Doc}.


handle_message(_Msg, _Doc) ->
    ignored.



'_get_content'(_DocPid, _DocKey) ->
    {error, not_implemented}.


'_get_and_watch'(_DocPid, _DocKey, _WatcherPid) ->
    {error, not_implemented}.


'_register_interest'(_DocPid, _DocKey, _WatcherPid) ->
    ok.


'_unregister_interest'(_DocPid, _DocKey, _WatcherPid) ->
    ok.


'_notify_change'(_WatcherPid, _DocKey, _Patch) ->
    ok.


'_notify_state'(_WatcherPid, _DocKey, _StateName) ->
    ok.


create_views(ViewSpecs, Children, Map) ->
    create_views(ViewSpecs, Children, Map, []).


create_views([], _Children, _Map, Acc) -> Acc;

create_views([{Id, PageSize, CompareFun} |Rem], Children, Map, Acc) ->
    View = erod_document_view:from_items(Children, PageSize, CompareFun, Map),
    create_views(Rem, Children, Map, [{Id, View} |Acc]).
