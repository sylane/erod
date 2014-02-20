-module(erod_document).

-export([new/1,
         get_content/2,
         get_children/4,
         patch_content/2,
         add_child/2]).

-define(Doc, ?MODULE).
-record(?Doc, {controller,
               children,
               views}).


new(Controller) ->
    #?Doc{controller = Controller,
          children = erod_maps:new(),
          views = erod_maps:new()}.


get_content(_FromVer, _Doc) ->
    undefined.


get_children(ViewName, PageId, FromVer, Doc) ->
    #?Doc{children = Children, views = Views} = Doc,
    case erod_maps:lookup(ViewName, Views) of
        none -> view_not_found;
        {value, View} ->
            erod_document_view:get_content(PageId, FromVer, Children, View)
    end.


patch_content(_Patch, Doc) ->
    Doc.


add_child(_Child, Doc) ->
    Doc.
