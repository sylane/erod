-module(erodws_proto_get_children_result).

-include_lib("erod/include/erod_document.hrl").

-export([encode/2]).


encode(jsx, unchanged) -> undefined;

encode(jsx, #erod_page{type = entity} = Page) ->
    #erod_page{key = Key, ver = Ver, view = ViewId, page = PageId,
               size = PageSize, total = TotalSize, data = Data} = Page,
    [{<<"key">>, erodlib_jsx:key_value(key, Key)},
     {<<"ver">>, erodlib_jsx:ver_value(ver, Ver)},
     {<<"view">>, erodlib_jsx:atom_value(view, ViewId)},
     {<<"page">>, erodlib_jsx:integer_value(page, PageId)},
     {<<"size">>, erodlib_jsx:integer_value(size, PageSize)},
     {<<"total">>, erodlib_jsx:integer_value(total, TotalSize)},
     {<<"page">>, [[{<<"key">>, erodlib_jsx:key_value(key, K)}
                    |erodlib_jsx:struct_value(page, V)]
                   || {K, V} <- Data]}];

encode(jsx, #erod_page{type = patch} = Page) ->
    #erod_page{key = Key, ver = Ver, view = ViewId, page = PageId,
               size = PageSize, total = TotalSize, data = Data} = Page,
    [{<<"key">>, erodlib_jsx:key_value(key, Key)},
     {<<"ver">>, erodlib_jsx:ver_value(ver, Ver)},
     {<<"view">>, erodlib_jsx:atom_value(view, ViewId)},
     {<<"page">>, erodlib_jsx:integer_value(page, PageId)},
     {<<"size">>, erodlib_jsx:integer_value(size, PageSize)},
     {<<"total">>, erodlib_jsx:integer_value(total, TotalSize)},
     {<<"patch">>, erodlib_jsx:patch_value(page, Data)}].
