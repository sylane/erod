-module(erodws_proto_get_content_result).

-include_lib("erod/include/erod_document.hrl").

-export([encode/2]).


encode(jsx, unchanged) -> undefined;

encode(jsx, #erod_content{type = entity} = Content) ->
    #erod_content{key = Key, ver = Ver, data = Data} = Content,
    [{<<"key">>, erodlib_jsx:key_value(key, Key)},
     {<<"ver">>, erodlib_jsx:ver_value(ver, Ver)},
     {<<"content">>, erodlib_jsx:struct_value(content, Data)}];

encode(jsx, #erod_content{type = patch} = Content) ->
    #erod_content{key = Key, ver = Ver, data = Data} = Content,
    [{<<"key">>, erodlib_jsx:key_value(key, Key)},
     {<<"ver">>, erodlib_jsx:ver_value(ver, Ver)},
     {<<"patch">>, erodlib_jsx:patch_value(patch, Data)}].
