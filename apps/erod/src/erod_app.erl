-module(erod_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile(
                 [ {'_', [{'_', erod_connection, []}]} ]),
    case cowboy:start_http(http_listener, 100, [{port, 8888}],
                           [{env, [{dispatch, Dispatch}]}]) of
        {ok, _Pid} -> erod_sup:start_link();
        {error, _Reason} = Error -> Error
    end.


stop(_State) ->
    ok.
