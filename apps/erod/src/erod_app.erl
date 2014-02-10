-module(erod_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [ {'_', [{'_', erod_connection, []}]} ]),
    cowboy:start_http(http_listener, 100, [{port, 8888}],
                      [{env, [{dispatch, Dispatch}]}]),
    erod_sup:start_link().


stop(_State) ->
    ok.
