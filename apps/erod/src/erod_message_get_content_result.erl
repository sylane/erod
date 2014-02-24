-module(erod_message_get_content_result).

-include("erod_internal.hrl").

-export([encode/2]).


encode(jsx, #?MsgGetConRes{type = entity} = Res) ->
    #?MsgGetConRes{key = Key, ver = Ver, content = Content} = Res,
    [{<<"key">>, erod_jsx:key_value(key, Key)},
     {<<"ver">>, erod_jsx:key_value(ver, Ver)},
     {<<"content">>, erod_jsx:struct_value(content, Content)}];

encode(jsx, #?MsgGetConRes{type = patch} = Res) ->
    #?MsgGetConRes{key = Key, ver = Ver, content = Patch} = Res,
    [{<<"key">>, erod_jsx:key_value(key, Key)},
     {<<"ver">>, erod_jsx:key_value(ver, Ver)},
     {<<"patch">>, erod_jsx:patch_value(patch, Patch)}].
