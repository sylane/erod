-module(erodlib_props).

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
         ensure_undefined/2]).


decode(jsx, Jsx) -> decode_jsx_value(Jsx).


encode(jsx, Props) -> encode_jsx_value(Props).



get_atom(Key, Props) ->
    bin2atom(Key, lookup(Key, Props)).


get_integer(Key, Props) ->
    ensure_integer(Key, lookup(Key, Props)).


get_allowed_atom(Key, Props, Allowed) ->
    allowed(Key, bin2atom(Key, lookup(Key, Props)), Allowed).


get_binary(Key, Props) ->
    ensure_bin(Key, lookup(Key, Props)).


get_binary(Key, Props, Default) ->
    ensure_bin(Key, lookup(Key, Props), Default).


get_struct(Key, Props) ->
    ensure_struct(Key, lookup(Key, Props)).


get_struct(Key, Props, Default) ->
    ensure_struct(Key, lookup(Key, Props), Default).


get_key(Key, Props) ->
    struct2key(Key, lookup(Key, Props)).


get_ver(Key, Props, Default) ->
    struct2ver(Key, lookup(Key, Props), Default).


get_bool(Key, Props, Default) ->
    ensure_bool(Key, lookup(Key, Props), Default).


ensure_undefined(Key, Props) ->
    case lookup(Key, Props) of
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
    erodlib:maybe_atom(K).


encode_jsx_value([{}]) -> [{}];

encode_jsx_value([T |_] = M) when is_tuple(T) ->
    [{encode_jsx_key(K), encode_jsx_value(V)} || {K, V} <- M];

encode_jsx_value(L) when is_list(L) ->
    [encode_jsx_value(V) || V <- L];

encode_jsx_value(V) -> V.


encode_jsx_key(K) when is_binary(K)  -> K;

encode_jsx_key(K) when is_atom(K)  ->
    erodlib:atom2bin(K).


lookup(Key, Props) when is_list(Props) ->
    lists:keyfind(Key, 1, Props);

lookup(_Key, _Props) ->
    error({format_error, bad_structure}).


bin2atom(Key, {Key, Value}) when is_binary(Value) ->
    try erodlib:bin2atom(Value) of
        Atom -> Atom
    catch
        error:badarg ->
            error({format_error, {value_not_allowed, Key}})
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


struct2key(Key, {Key, Props}) ->
    Type = get_atom(type, Props),
    Id = get_struct(id, Props),
    build_key(Key, Type, Id);

struct2key(Key, false) ->
    error({format_error, {key_required, Key}}).


struct2ver(Key, {Key, null}, Default) ->
    Default;

struct2ver(Key, {Key, Props}, _Default) ->
    parse_ident_list(Key, Props);

struct2ver(_Key, false, Default) ->
    Default.


build_key(_Key, Type, Num) when is_integer(Num) -> {Type, Num};

build_key(_Key, Type, Bin) when is_binary(Bin) -> {Type, Bin};

build_key(Key, Type, List) when is_list(List) ->
    {Type, parse_ident_list(Key, List)};

build_key(Key, _Type, _List) ->
    error({format_error, {bad_value_type, Key}}).


parse_ident_list(Key, []) ->
    error({format_error, {bad_value_type, Key}});

parse_ident_list(Key, List) when is_list(List) ->
    case lists:all(fun(V) -> is_integer(V) or is_binary(V) end, List) of
        false -> error({format_error, {bad_value_type, Key}});
        true -> list_to_tuple(List)
    end;

parse_ident_list(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).
