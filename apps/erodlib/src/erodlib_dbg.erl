%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erodlib.
%%%
%%% Erodlib is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% ==========================================================================
%%% @copyright 2014 Sebastien Merle <s.merle@gmail.com>
%%% @author Sebastien Merle <s.merle@gmail.com>
%%% @doc TODO: Document module erodlib_dbg.
%%% @end
%%% ==========================================================================

-module(erodlib_dbg).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Shortcut functions
-export([tm/1, cm/1]).

%%% API Functions
-export([trace_module/1,
         clear_module/1]).


%%% ==========================================================================
%%% Shortcut Functions
%%% ==========================================================================

tm(Module) -> trace_module(Module).


cm(Module) -> clear_module(Module).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

trace_module(Module) when is_atom(Module) ->
    setup_tracer(),
    dbg:tpl(Module, x).


clear_module(Module) when is_atom(Module) ->
    dbg:ctpl(Module).



%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

setup_tracer() ->
    case dbg:tracer() of
        {ok, _Pid} ->
            dbg:p(all, c),
            ok;
        {error, already_started} ->
            ok
    end.
