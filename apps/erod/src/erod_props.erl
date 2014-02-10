-module(erod_props).

-export([decode/2]).
-export([encode/2]).
-export([get_atom/2]).
-export([get_allowed_atom/3]).
-export([get_binary/2, get_binary/3]).
-export([get_struct/3]).
-export([ensure_undefined/2]).


decode(jsx, Jsx) -> decode_jsx_value(Jsx).


encode(jsx, Props) -> encode_jsx_value(Props).



get_atom(Key, Props) ->
    bin2atom(Key, lookup(Key, Props)).


get_allowed_atom(Key, Props, Allowed) ->
    allowed(Key, bin2atom(Key, lookup(Key, Props)), Allowed).


get_binary(Key, Props) ->
    bin2bin(Key, lookup(Key, Props)).


get_binary(Key, Props, Default) ->
    bin2bin(Key, lookup(Key, Props), Default).


get_struct(Key, Props, Default) ->
    any2struct(Key, lookup(Key, Props), Default).


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
    erod_common:maybe_atom(K).


encode_jsx_value([{}]) -> [{}];

encode_jsx_value([T |_] = M) when is_tuple(T) ->
    [{encode_jsx_key(K), encode_jsx_value(V)} || {K, V} <- M];

encode_jsx_value(L) when is_list(L) ->
    [encode_jsx_value(V) || V <- L];

encode_jsx_value(V) -> V.


encode_jsx_key(K) when is_binary(K)  -> K;

encode_jsx_key(K) when is_atom(K)  ->
    erod_common:atom2bin(K).


lookup(Key, Props) when is_list(Props) ->
    lists:keyfind(Key, 1, Props);

lookup(_Key, _Props) ->
    error({format_error, bad_structure}).


bin2atom(Key, {Key, Value}) when is_binary(Value) ->
    try erod_common:bin2atom(Value) of
        Atom -> Atom
    catch
        error:badarg ->
            error({format_error, {value_not_allowed, Key}})
    end;

bin2atom(Key, {Key, _Value}) ->
    error({format_error, {bad_value_type, Key}});

bin2atom(Key, false) ->
    error({format_error, {key_required, Key}}).


bin2bin(Key, {Key, Value})
  when is_binary(Value) -> Value;

bin2bin(Key, {Key, _Value}) ->
    error({format_error, {bad_value_type, Key}});

bin2bin(Key, false) ->
    error({format_error, {key_required, Key}}).


bin2bin(Key, {Key, Value}, _Default)
  when is_binary(Value) -> Value;

bin2bin(Key, {Key, _Value}, _Default) ->
    error({format_error, {bad_value_type, Key}});

bin2bin(_Key, false, Default) ->
    Default.


any2struct(Key, {Key, Value}, _Default) ->
    Value;

any2struct(_Key, false, Default) ->
    Default.


allowed(Key, Value, Allowed) ->
    case lists:member(Value, Allowed) of
        true -> Value;
        false -> error({format_error, {value_not_allowed, Key}})
    end.
