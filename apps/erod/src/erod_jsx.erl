-module(erod_jsx).

-export([binary_value/2]).
-export([atom_value/2]).
-export([allowed_atom_value/3]).
-export([integer_value/2]).
-export([struct_value/2]).


binary_value(_Key, Bin) when is_binary(Bin) -> Bin;

binary_value(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


atom_value(_Key, Atom) when is_atom(Atom) ->
    erlang:atom_to_binary(Atom, utf8);

atom_value(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


allowed_atom_value(Key, Atom, Allowed) when is_atom(Atom) ->
    case lists:member(Atom, Allowed) of
        true -> erlang:atom_to_binary(Atom, utf8);
        false -> error({format_error, {value_not_allowed, Key}})
    end;

allowed_atom_value(Key, _Other, _Allowed) ->
    error({format_error, {bad_value_type, Key}}).


integer_value(_Key, Value) when is_integer(Value) -> Value;

integer_value(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


struct_value(_Key, Value) when is_integer(Value) ->
    % We accept integers for the key identifiers
    Value;

struct_value(_Key, Rec) when is_tuple(Rec) ->
    Rec:encode(jsx);

struct_value(_Key, Props) when is_list(Props) ->
    % We assume that if it is not a record it is a proplist
    erod_props:encode(jsx, Props);

struct_value(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


