-module(erod_message_get_children_result).

-include("erodws_internal.hrl").

-export([encode/2]).


encode(jsx, #?MsgGetChiRes{page = #erod_page{type = entity}} = Res) ->
    #?MsgGetChiRes{page = Page} = Res,
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

encode(jsx, #?MsgGetChiRes{page = #erod_page{type = patch}} = Res) ->
    #?MsgGetChiRes{page = Page} = Res,
    #erod_page{key = Key, ver = Ver, view = ViewId, page = PageId,
               size = PageSize, total = TotalSize, data = Data} = Page,
    [{<<"key">>, erodlib_jsx:key_value(key, Key)},
     {<<"ver">>, erodlib_jsx:ver_value(ver, Ver)},
     {<<"view">>, erodlib_jsx:atom_value(view, ViewId)},
     {<<"page">>, erodlib_jsx:integer_value(page, PageId)},
     {<<"size">>, erodlib_jsx:integer_value(size, PageSize)},
     {<<"total">>, erodlib_jsx:integer_value(total, TotalSize)},
     {<<"patch">>, erodlib_jsx:patch_value(page, Data)}].
