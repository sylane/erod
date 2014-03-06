-module(erodlib_term).

-export([decode/2,
         encode/2,
         get_atom/2,
         get_integer/2,
         get_allowed_atom/3,
         get_binary/2,
         get_binary/3,
         get_struct/2,
         get_struct/3,
         get_key/2,
         get_ver/3,
         get_bool/3,
         get_patch/2,
         ensure_undefined/2]).


decode(jsx, Jsx) -> decode_jsx_value(Jsx).


encode(jsx, Term) -> encode_jsx_value(Term).



get_atom(Key, Term) ->
    bin2atom(Key, lookup(Key, Term)).


get_integer(Key, Term) ->
    ensure_integer(Key, lookup(Key, Term)).


get_allowed_atom(Key, Term, Allowed) ->
    allowed(Key, bin2atom(Key, lookup(Key, Term)), Allowed).


get_binary(Key, Term) ->
    ensure_bin(Key, lookup(Key, Term)).


get_binary(Key, Term, Default) ->
    ensure_bin(Key, lookup(Key, Term), Default).


get_struct(Key, Term) ->
    ensure_struct(Key, lookup(Key, Term)).


get_struct(Key, Term, Default) ->
    ensure_struct(Key, lookup(Key, Term), Default).


get_key(Key, Term) ->
    struct2key(Key, lookup(Key, Term)).


get_ver(Key, Term, Default) ->
    struct2ver(Key, lookup(Key, Term), Default).


get_bool(Key, Term, Default) ->
    ensure_bool(Key, lookup(Key, Term), Default).


get_patch(Key, Term) ->
    struct2patch(Key, lookup(Key, Term)).


ensure_undefined(Key, Term) ->
    case lookup(Key, Term) of
        false -> undefined;
        _ -> error({format_error, {key_not_allowed, Key}})
    end.




decode_jsx_value([{}]) -> [{}];

decode_jsx_value([T |_] = M) when is_tuple(T) ->
    [{decode_jsx_key(K), decode_jsx_value(V)} || {K, V} <- M];

decode_jsx_value(L) when is_list(L) ->
    [decode_jsx_value(V) || V <- L];

decode_jsx_value(V) -> V.


decode_jsx_key(K) ->
    erodlib:maybe_atom(K, K).


encode_jsx_value([{}]) -> [{}];

encode_jsx_value([T |_] = M) when is_tuple(T) ->
    [{encode_jsx_key(K), encode_jsx_value(V)} || {K, V} <- M];

encode_jsx_value(L) when is_list(L) ->
    [encode_jsx_value(V) || V <- L];

encode_jsx_value(V) -> V.


encode_jsx_key(K) when is_binary(K)  -> K;

encode_jsx_key(K) when is_atom(K)  ->
    erodlib:atom2bin(K).


lookup(Key, Term) when is_list(Term) ->
    lists:keyfind(Key, 1, Term);

lookup(_Key, _Term) ->
    error({format_error, bad_structure}).


bin2atom(Key, {Key, Value}) when is_binary(Value) ->
    case erodlib:maybe_atom(Value, {}) of
        {} -> error({format_error, {value_not_allowed, Key}});
        Atom -> Atom
    end;

bin2atom(Key, {Key, _Value}) ->
    error({format_error, {bad_value_type, Key}});

bin2atom(Key, false) ->
    error({format_error, {key_required, Key}}).


ensure_integer(Key, {Key, Value}) when is_integer(Value) -> Value;

ensure_integer(Key, {Key, _Value}) ->
    error({format_error, {bad_value_type, Key}});

ensure_integer(Key, false) ->
    error({format_error, {key_required, Key}}).


ensure_bin(Key, {Key, Value})
  when is_binary(Value) -> Value;

ensure_bin(Key, {Key, _Value}) ->
    error({format_error, {bad_value_type, Key}});

ensure_bin(Key, false) ->
    error({format_error, {key_required, Key}}).


ensure_bin(Key, {Key, Value}, _Default)
  when is_binary(Value) -> Value;

ensure_bin(Key, {Key, _Value}, _Default) ->
    error({format_error, {bad_value_type, Key}});

ensure_bin(_Key, false, Default) ->
    Default.


ensure_struct(Key, {Key, Value}) ->
    Value;

ensure_struct(Key, false) ->
    error({format_error, {key_required, Key}}).


ensure_struct(Key, {Key, Value}, _Default) ->
    Value;

ensure_struct(_Key, false, Default) ->
    Default.


ensure_bool(_Key, false, Default) -> Default;

ensure_bool(Key, {Key, Bool}, _Default) when is_boolean(Bool) -> Bool;

ensure_bool(Key, {Key, _Value}, _Default) ->
    error({format_error, {bad_value_type, Key}}).


allowed(Key, Value, Allowed) ->
    case lists:member(Value, Allowed) of
        true -> Value;
        false -> error({format_error, {value_not_allowed, Key}})
    end.


struct2key(Key, {Key, Term}) ->
    Type = get_atom(type, Term),
    Id = get_struct(id, Term),
    build_key(Key, Type, Id);

struct2key(Key, false) ->
    error({format_error, {key_required, Key}}).


struct2ver(Key, {Key, null}, Default) ->
    Default;

struct2ver(Key, {Key, Term}, _Default) ->
    decode_ident_list(Key, Term);

struct2ver(_Key, false, Default) ->
    Default.


struct2patch(Key, {Key, List}) when is_list(List) ->
    decode_patch(Key, List);

struct2patch(Key, {Key, _Other}) ->
    error({format_error, {bad_value_type, Key}});

struct2patch(Key, false) ->
    error({format_error, {key_required, Key}}).


build_key(_Key, Type, Num) when is_integer(Num) -> {Type, Num};

build_key(_Key, Type, Bin) when is_binary(Bin) -> {Type, Bin};

build_key(Key, Type, List) when is_list(List) ->
    {Type, decode_ident_list(Key, List)};

build_key(Key, _Type, _List) ->
    error({format_error, {bad_value_type, Key}}).


decode_ident_list(Key, []) ->
    error({format_error, {bad_value_type, Key}});

decode_ident_list(Key, List) when is_list(List) ->
    case lists:all(fun(V) -> is_integer(V) or is_binary(V) end, List) of
        false -> error({format_error, {bad_value_type, Key}});
        true -> list_to_tuple(List)
    end;

decode_ident_list(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


decode_patch(Key, List) ->
    [decode_patch_entry(Key, E) || E <- List].


decode_patch_entry(Key, Entry) ->
    decode_patch_entry(Key, get_atom(op, Entry), Entry).


decode_patch_entry(_Key, remove, Entry) ->
    Path = decode_patch_path(path, get_struct(path, Entry)),
    {remove, Path};

decode_patch_entry(_Key, Op, Entry) when Op =:= add; Op =:= replace ->
    Path = decode_patch_path(path, get_struct(path, Entry)),
    Value = get_struct(value, Entry),
    {Op, Path, Value};

decode_patch_entry(_Key, Op, Entry) when Op =:= move; Op =:= copy ->
    From = decode_patch_path(from, get_struct(path, Entry)),
    Path = decode_patch_path(path, get_struct(path, Entry)),
    {Op, From, Path};

decode_patch_entry(Key, _Op, _Entry) ->
    error({format_error, {value_not_allowed, Key}}).


decode_patch_path(_Key, Path) when is_binary(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>> |PathItems] ->
            [decode_patch_path_item(I) || I <- PathItems];
        _Any -> error({format_error, {value_not_allowed}})
    end;

decode_patch_path(Key, _Path) ->
    error({format_error, {bad_value_type, Key}}).


decode_patch_path_item(Item1) ->
    Item2 = binary:replace(Item1, <<"~1">>, <<"/">>, [global]),
    Item3 = binary:replace(Item2, <<"~0">>, <<"~">>, [global]),
    try binary_to_integer(Item3) catch error:badarg -> Item3 end.
