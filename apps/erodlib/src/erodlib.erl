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
%%% @doc TODO: Document module erodlib.
%%% @end
%%% ==========================================================================

-module(erodlib).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([maybe_atom/2,
         bin2atom/1,
         atom2bin/1,
         peer2bin/1]).


%%% ==========================================================================
%%% Types
%%% ==========================================================================

-type indice() :: erodlib_indices:indice().
-type emap() :: erodlib_maps:emap().
-type eset() :: erodlib_sets:eset().
-type compare_fun() :: erodlib_indices:compare_fun().
-type map_fun() :: erodlib_indices:map_fun().

-type erod_key_part() :: 0 | pos_integer() | binary().
-type erod_key() :: {atom(), erod_key_part() | list(erod_key_part())}.
-type erod_ver_part() :: 0 | pos_integer() | binary().
-type erod_version() :: list(erod_ver_part()).
-type erod_patch_path() :: [atom() | pos_integer()].
-type erod_patch_entry() :: {remove, erod_patch_path()}
                          | {add | replace, erod_patch_path(), term()}
                          | {move | copy, erod_patch_path(), erod_patch_path()}.
-type erod_patch() :: [erod_patch_entry()] | [].

-export_type([indice/0, emap/0, eset/0, compare_fun/0, map_fun/0,
              erod_key/0, erod_version/0, erod_patch/0]).



%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Converts a binary to an atom if it exists or a default value if not.
%% @end
%% -----------------------------------------------------------------
-spec maybe_atom(Value, Default) -> Result
    when Value :: binary(), Default :: term(), Result :: atom() | term().
%% -----------------------------------------------------------------

maybe_atom(Value, Default) when is_binary(Value) ->
    try erlang:binary_to_existing_atom(Value, utf8) of
        Atom -> Atom
    catch
        error:badarg -> Default
    end.


%% -----------------------------------------------------------------
%% @doc Converts a binary to an atom. Could potentially fill the atom table.
%% @end
%% -----------------------------------------------------------------
-spec bin2atom(Value) -> Result
    when Value :: binary() | atom(), Result :: atom().
%% -----------------------------------------------------------------

bin2atom(Value) when is_binary(Value) ->
    erlang:binary_to_atom(Value, utf8).


atom2bin(Value) when is_atom(Value) ->
    erlang:atom_to_binary(Value, utf8).


%% -----------------------------------------------------------------
%% @doc Converts a network peer tuple to a binary if possile.
%% This function should never failed, it is though for logging.
%% @end
%% -----------------------------------------------------------------
-spec peer2bin(Value) -> Result
    when Value :: {inet:ip_address(), inet:port_number()} | undefined,
         Result :: binary().
%% -----------------------------------------------------------------

peer2bin(undefined) -> <<"unknown">>;

peer2bin({Addr, Port}) when is_tuple(Addr), is_integer(Port) ->
    AddrBin = list_to_binary(inet_parse:ntoa(Addr)),
    PortBin = integer_to_binary(Port),
    <<AddrBin/binary, ":", PortBin/binary>>.
