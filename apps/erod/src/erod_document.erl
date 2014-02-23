-module(erod_document).

-export([new/3,
         get_content/2,
         get_children/4,
         patch_content/2,
         add_child/2,
         patch_child/3]).

-export([watch/1]).

-export([register_interest/3,
         unregister_interest/3,
         notify_change/3,
         notify_state/3]).

-export([handle_info/2]).

-define(Doc, ?MODULE).
-record(?Doc, {key,
               sub_mod,
               sub_state,
               content,
               children,
               verlog,
               views}).


new(DocKey, Module, Options) ->
    case Module:init(Options, #?Doc{key = DocKey}) of
        {ok, Content, Children, ViewSpecs, State, Doc} ->
            ChildrenMap = erod_maps:from_items(Children),
            Views = create_views(ViewSpecs, Children, ChildrenMap),
            ViewMap = erod_maps:from_items(Views),
            erod_document_manager:register_document(DocKey, self()),
            Doc#?Doc{sub_mod = Module,
                     sub_state = State,
                     content = Content,
                     children = ChildrenMap,
                     verlog = erod_document_verlog:new(),
                     views = ViewMap}
    end.


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


watch(DocKeyOrKeys) ->
    erod_document_manager:register_interest(DocKeyOrKeys, self()).


register_interest(_DocPid, _DocKey, _WatcherPid) ->
    ok.


unregister_interest(_DocPid, _DocKey, _WatcherPid) ->
    ok.


notify_change(_WatcherPid, _DocKey, _Patch) ->
    ok.


notify_state(_WatcherPid, _DocKey, _StateName) ->
    ok.


handle_info(_Msg, Doc) ->
    {pass, Doc}.



create_views(ViewSpecs, Children, Map) ->
    create_views(ViewSpecs, Children, Map, []).


create_views([], _Children, _Map, Acc) -> Acc;

create_views([{Id, PageSize, CompareFun} |Rem], Children, Map, Acc) ->
    View = erod_document_view:from_items(Children, PageSize, CompareFun, Map),
    create_views(Rem, Children, Map, [{Id, View} |Acc]).
