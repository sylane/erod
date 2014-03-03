-module(erodws).

-export([start/1,
         start/3]).


start(Port) ->
    start(Port, erodws_protocol_default, []).


start(Port, Protocol, Options) when is_integer(Port) ->
    Dispatch = cowboy_router:compile(
                 [ {'_', [{'_', erodws_connection, {Protocol, Options}}]} ]),
    cowboy:start_http(http_listener, 100, [{port, 8888}],
                      [{env, [{dispatch, Dispatch}]}]).
