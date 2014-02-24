-module(erod).

-export([start_document/3,
         start_websocket/1]).


start_websocket(Port) when is_integer(Port) ->
    Dispatch = cowboy_router:compile(
                 [ {'_', [{'_', erod_connection, []}]} ]),
    cowboy:start_http(http_listener, 100, [{port, 8888}],
                      [{env, [{dispatch, Dispatch}]}]).


start_document(DocKey, Factory, Options) ->
    erod_document_sup:start_child(DocKey, Factory, Options).
