-module(erdom_app).

-behaviour(application).

-export([start/2, stop/1]).


start(_StartType, _StartArgs) ->
    erod_document_manager:add_locator(user, dummy_locator, []),
    case erod:start_websocket(8888) of
        {ok, _Pid} -> erdom_sup:start_link();
        {error, _Reason} = Error -> Error
    end.


stop(_State) ->
    ok.
