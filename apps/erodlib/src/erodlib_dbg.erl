-module(erodlib_dbg).

-export([tm/1, cm/1]).

-export([trace_module/1]).
-export([clear_module/1]).


tm(Module) -> trace_module(Module).


cm(Module) -> clear_module(Module).


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
