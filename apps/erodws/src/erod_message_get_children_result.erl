-module(erod_message_get_children_result).

-include("erodws_internal.hrl").

-export([encode/2]).


encode(jsx, #?MsgGetChiRes{page = #erod_page{type = entity}} = Res) ->
    #?MsgGetChiRes{page = Page} = Res,
    #erod_page{key = Key, ver = Ver, view = ViewId, page = PageId,
               size = PageSize, total = TotalSize, data = Data} = Page,
    [{<<"key">>, erod_jsx:key_value(key, Key)},
     {<<"ver">>, erod_jsx:ver_value(ver, Ver)},
     {<<"view">>, erod_jsx:atom_value(view, ViewId)},
     {<<"page">>, erod_jsx:integer_value(page, PageId)},
     {<<"size">>, erod_jsx:integer_value(size, PageSize)},
     {<<"total">>, erod_jsx:integer_value(total, TotalSize)},
     {<<"page">>, [[{<<"key">>, erod_jsx:key_value(key, K)}
                    |erod_jsx:struct_value(page, V)]
                   || {K, V} <- Data]}];

encode(jsx, #?MsgGetChiRes{page = #erod_page{type = patch}} = Res) ->
    #?MsgGetChiRes{page = Page} = Res,
    #erod_page{key = Key, ver = Ver, view = ViewId, page = PageId,
               size = PageSize, total = TotalSize, data = Data} = Page,
    [{<<"key">>, erod_jsx:key_value(key, Key)},
     {<<"ver">>, erod_jsx:ver_value(ver, Ver)},
     {<<"view">>, erod_jsx:atom_value(view, ViewId)},
     {<<"page">>, erod_jsx:integer_value(page, PageId)},
     {<<"size">>, erod_jsx:integer_value(size, PageSize)},
     {<<"total">>, erod_jsx:integer_value(total, TotalSize)},
     {<<"patch">>, erod_jsx:patch_value(page, Data)}].
