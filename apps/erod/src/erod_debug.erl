-module(erod_debug).

-export([trace_module/1]).
-export([clear_module/1]).


trace_module(Module) when is_atom(Module) ->
    setup_tracer(),
    dbg:tpl(Module, x).


clear_module(Module) when is_atom(Module) ->
    dbg:ctpl(Module).


setup_tracer() ->
    case dbg:tracer() of
        {ok, _Pid} ->
            dbg:p(all, c),
            ok;
        {error, already_started} ->
            ok
    end.
