-module(erod_jsx).

-export([binary_value/2,
         atom_value/2,
         allowed_atom_value/3,
         integer_value/2,
         struct_value/2,
         key_value/2,
         ver_value/2,
         patch_value/2]).


binary_value(_Key, Bin) when is_binary(Bin) -> Bin;

binary_value(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


atom_value(_Key, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);

atom_value(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


allowed_atom_value(Key, Atom, Allowed) when is_atom(Atom) ->
    case lists:member(Atom, Allowed) of
        true -> atom_to_binary(Atom, utf8);
        false -> error({format_error, {value_not_allowed, Key}})
    end;

allowed_atom_value(Key, _Other, _Allowed) ->
    error({format_error, {bad_value_type, Key}}).


integer_value(_Key, Value) when is_integer(Value) -> Value;

integer_value(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


struct_value(_Key, undefined) -> null;

struct_value(_Key, Bin) when is_binary(Bin) -> Bin;

struct_value(_Key, Bool) when is_boolean(Bool) -> Bool;

struct_value(_Key, Num) when is_integer(Num) -> Num;

struct_value(_Key, Rec) when is_tuple(Rec) ->
    Rec:encode(jsx);

struct_value(_Key, Props) when is_list(Props) ->
    % We assume that if it is not a record it is a proplist
    erod_props:encode(jsx, Props);

struct_value(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


key_value(_Key, undefined) -> null;

key_value(_Key, {Type, Num}) when is_integer(Num) ->
    [{<<"type">>, atom_to_binary(Type, utf8)}, {<<"id">>, Num}];

key_value(_Key, {Type, Bin}) when is_binary(Bin) ->
    [{<<"type">>, atom_to_binary(Type, utf8)}, {<<"id">>, Bin}];

key_value(Key, {Type, Tup}) when is_tuple(Tup) ->
    [{<<"type">>, atom_to_binary(Type, utf8)},
     {<<"id">>, [encode_ident_part(Key, V) || V <- tuple_to_list(Tup)]}];

key_value(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


ver_value(Key, Tup) when is_tuple(Tup) ->
    [encode_ident_part(Key, V) || V <- tuple_to_list(Tup)];

ver_value(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


patch_value(Key, Patch) ->
    encode_patch(Key, Patch, []).


encode_ident_part(_Key, Num) when is_integer(Num) -> Num;

encode_ident_part(_Key, Bin) when is_binary(Bin) -> Bin;

encode_ident_part(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


encode_patch(_Key, [], Acc) -> lists:reverse(Acc);

encode_patch(Key, [{remove, Path} |Rem], Acc) ->
    Entry = [{<<"op">>, <<"remove">>},
             {<<"path">>, encode_patch_path(Key, Path)}],
    encode_patch(Key, Rem, [Entry |Acc]);

encode_patch(Key, [{Op, Path, Value} |Rem], Acc)
  when Op =:= add; Op =:= replace ->
    Entry = [{<<"op">>, atom_to_binary(Op, utf8)},
             {<<"path">>, encode_patch_path(Key, Path)},
             {<<"value">>, struct_value(Key, Value)}],
    encode_patch(Key, Rem, [Entry |Acc]);

encode_patch(Key, [{Op, From, Path} |Rem], Acc)
  when Op =:= move; Op =:= copy ->
    Entry = [{<<"op">>, atom_to_binary(Op, utf8)},
             {<<"from">>, encode_patch_path(Key, From)},
             {<<"path">>, struct_value(Key, Path)}],
    encode_patch(Key, Rem, [Entry |Acc]).


encode_patch_path(Key, Path) ->
    iolist_to_binary(patch_path_to_list(Key, Path)).


patch_path_to_list(_Key, []) -> [];

patch_path_to_list(Key, [Atom |Rem]) when is_atom(Atom) ->
    [$/, atom_to_list(Atom) |patch_path_to_list(Key, Rem)];

patch_path_to_list(Key, [Num |Rem]) when is_integer(Num) ->
    [$/, integer_to_list(Num) |patch_path_to_list(Key, Rem)];

patch_path_to_list(Key, _Path) ->
    error({format_error, {bad_value_type, Key}}).
