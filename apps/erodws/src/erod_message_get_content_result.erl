-module(erod_message_get_content_result).

-include("erodws_internal.hrl").

-export([encode/2]).


encode(jsx, #?MsgGetConRes{content = #erod_content{type = entity}} = Res) ->
    #?MsgGetConRes{content = Content} = Res,
    #erod_content{key = Key, ver = Ver, data = Data} = Content,
    [{<<"key">>, erod_jsx:key_value(key, Key)},
     {<<"ver">>, erod_jsx:ver_value(ver, Ver)},
     {<<"content">>, erod_jsx:struct_value(content, Data)}];

encode(jsx, #?MsgGetConRes{content = #erod_content{type = patch}} = Res) ->
    #?MsgGetConRes{content = Content} = Res,
    #erod_content{key = Key, ver = Ver, data = Data} = Content,
    [{<<"key">>, erod_jsx:key_value(key, Key)},
     {<<"ver">>, erod_jsx:ver_value(ver, Ver)},
     {<<"patch">>, erod_jsx:patch_value(patch, Data)}].
