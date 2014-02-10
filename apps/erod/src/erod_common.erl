-module(erod_common).

-export([maybe_atom/1]).
-export([bin2atom/1]).
-export([atom2bin/1]).


maybe_atom(B) when is_binary(B) ->
    try erlang:binary_to_existing_atom(B, utf8) of
        Atom -> Atom
    catch
        error:badarg -> B
    end.


bin2atom(B) when is_binary(B) ->
    erlang:binary_to_atom(B, utf8).


atom2bin(A) when is_atom(A) ->
    erlang:atom_to_binary(A, utf8).
