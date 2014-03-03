-module(erodws_proxy).

-behaviour(erod_proxy).

-export([new/1]).

-export([init/1,
         accept/3,
         handle_info/3,
         notify/5]).

-define(St, ?MODULE).
-record(?St, {conn}).


new(Connection) ->
    erod_proxy:new(?MODULE, [Connection]).


init([Conn]) ->
     #?St{conn = Conn}.


accept(_Ctx, _Proxy, #?St{conn = Conn} = State) ->
    try erlang:link(Conn) of
        true -> {ok, State}
    catch
        _:badarg -> {eror, link_failed, State}
    end.


handle_info({'EXIT', Conn, Reason}, _Proxy, #?St{conn = Conn} = State) ->
    {dead, Reason, State};

handle_info(_, _Proxy, _State) -> ignored.


notify(Name, Fmt, Data, _Proxy, #?St{conn = Conn} = State) ->
    erodws_connection:notify(Conn, Name, Fmt, Data),
    State.
