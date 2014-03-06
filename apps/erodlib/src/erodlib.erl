-module(erodlib).

-export([maybe_atom/2,
         bin2atom/1,
         atom2bin/1,
         peer2bin/1]).


maybe_atom(B, Default) when is_binary(B) ->
    try erlang:binary_to_existing_atom(B, utf8) of
        Atom -> Atom
    catch
        error:badarg -> Default
    end.


bin2atom(B) when is_binary(B) ->
    erlang:binary_to_atom(B, utf8).


atom2bin(A) when is_atom(A) ->
    erlang:atom_to_binary(A, utf8).


peer2bin(undefined) -> <<"unknown">>;

peer2bin({Addr, Port}) when is_tuple(Addr), is_integer(Port) ->
    AddrBin = list_to_binary(inet_parse:ntoa(Addr)),
    PortBin = integer_to_binary(Port),
    <<AddrBin/binary, ":", PortBin/binary>>.
