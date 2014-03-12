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
%%% @doc TODO: Document module erodlib_term.
%%% @end
%%% ==========================================================================

-module(erodlib_term).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([decode/2,
         encode/2,
         get_atom/2,
         get_allowed_atom/3,
         get_int/2,
         get_bin/2, get_bin/3,
         get_any/2, get_any/3,
         get_key/2,
         get_ver/3,
         get_bool/3,
         get_patch/2,
         ensure_undefined/2]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Decodes data in specified format to erlang an terms.
%% The objects/dictionaries are decoded to property list where keys
%% may be binaries if there is no already existing atoms defined for them.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec decode(Format, Data) -> Term
    when Format :: jsx, Data :: term(), Term :: term().
%% -----------------------------------------------------------------

decode(jsx, Jsx) -> decode_jsx_value(Jsx).


%% -----------------------------------------------------------------
%% @doc Encodes an erlang term into specified format.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec encode(Format, Term) -> Data
    when Format :: jsx, Data :: term(), Term :: term().
%% -----------------------------------------------------------------

encode(jsx, Term) -> encode_jsx_value(Term).


%% -----------------------------------------------------------------
%% @doc Gets an atom value from an erlang term.
%%
%% If the term doesn't contains the specified key or the value cannot
%% be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_atom(Key, Term) -> Value
    when Key :: atom() | binary(), Term :: term(), Value :: atom().
%% -----------------------------------------------------------------

get_atom(Key, Term) ->
    bin2atom(Key, lookup(Key, Term)).


%% -----------------------------------------------------------------
%% @doc Gets an atom value from an erlang term if it is one
%% of the allowed values.
%%
%% If the term doesn't contains the specified key or the value cannot
%% be converted to the wanted type or the value is not allowed,
%% a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_allowed_atom(Key, Term, Allowed) -> Value
    when Key :: atom() | binary(), Term :: term(),
         Allowed :: [atom()], Value :: atom().
%% -----------------------------------------------------------------

get_allowed_atom(Key, Term, Allowed) ->
    allowed(Key, bin2atom(Key, lookup(Key, Term)), Allowed).


%% -----------------------------------------------------------------
%% @doc Gets an integer value from an erlang term.
%%
%% If the term doesn't contains the specified key or the value cannot
%% be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_int(Key, Term) -> Value
    when Key :: atom() | binary(), Term :: term(), Value :: integer().
%% -----------------------------------------------------------------

get_int(Key, Term) ->
    ensure_integer(Key, lookup(Key, Term)).


%% -----------------------------------------------------------------
%% @doc Gets a binary value from an erlang term.
%%
%% If the term doesn't contains the specified key or the value cannot
%% be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_bin(Key, Term) -> Value
    when Key :: atom() | binary(), Term :: term(), Value :: binary().
%% -----------------------------------------------------------------

get_bin(Key, Term) ->
    ensure_bin(Key, lookup(Key, Term)).


%% -----------------------------------------------------------------
%% @doc Gets a binary value from an erlang term if the key exists
%% or return a default value otherwise.
%%
%% If the value cannot be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_bin(Key, Term, Default) -> Value
    when Key :: atom() | binary(), Term :: term(),
         Value :: binary(), Default :: term().
%% -----------------------------------------------------------------

get_bin(Key, Term, Default) ->
    ensure_bin(Key, lookup(Key, Term), Default).


%% -----------------------------------------------------------------
%% @doc Gets a value from an erlang term.
%%
%% If the term doesn't contains the specified key, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_any(Key, Term) -> Value
    when Key :: atom() | binary(), Term :: term(), Value :: term().
%% -----------------------------------------------------------------

get_any(Key, Term) ->
    ensure_struct(Key, lookup(Key, Term)).


%% -----------------------------------------------------------------
%% @doc Gets a value from an erlang term if the key exists
%% or a default value otherwise.
%% @end
%% -----------------------------------------------------------------
-spec get_any(Key, Term, Default) -> Value
    when Key :: atom() | binary(), Term :: term(),
         Value :: term(), Default :: term().
%% -----------------------------------------------------------------

get_any(Key, Term, Default) ->
    ensure_struct(Key, lookup(Key, Term), Default).


%% -----------------------------------------------------------------
%% @doc Gets an erod key from an erlang term.
%%
%% If the term doesn't contains the specified key or the value cannot
%% be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_key(Key, Term) -> Value
    when Key :: atom() | binary(), Term :: term(), Value :: erodlib:erod_key().
%% -----------------------------------------------------------------

get_key(Key, Term) ->
    struct2key(Key, lookup(Key, Term)).


%% -----------------------------------------------------------------
%% @doc Gets an erod version from an erlang term if the key exists
%% or return a default value otherwise.
%%
%% If the value cannot be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_ver(Key, Term, Default) -> Value
    when Key :: atom() | binary(), Term :: term(),
         Value :: erodlib:erod_version(), Default :: term().
%% -----------------------------------------------------------------

get_ver(Key, Term, Default) ->
    struct2ver(Key, lookup(Key, Term), Default).


%% -----------------------------------------------------------------
%% @doc Gets a boolean value from an erlang term if the key exists
%% or return a default value otherwise.
%%
%% If the value cannot be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_bool(Key, Term, Default) -> Value
    when Key :: atom() | binary(), Term :: term(),
         Value :: boolean(), Default :: term().
%% -----------------------------------------------------------------

get_bool(Key, Term, Default) ->
    ensure_bool(Key, lookup(Key, Term), Default).


%% -----------------------------------------------------------------
%% @doc Gets an erod patch from an erlang term.
%%
%% If the term doesn't contains the specified key or the value cannot
%% be converted to the wanted type, a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec get_patch(Key, Term) -> Value
    when Key :: atom() | binary(), Term :: term(),
         Value :: erodlib:erod_patch().
%% -----------------------------------------------------------------

get_patch(Key, Term) ->
    struct2patch(Key, lookup(Key, Term)).


%% -----------------------------------------------------------------
%% @doc Ensures the specified key is not present in the term.
%%
%% If the term contains the specified key a format error is thrown.
%% @throws {format_error, Reason :: term()}
%% @end
%% -----------------------------------------------------------------
-spec ensure_undefined(Key, Term) -> undefined
    when Key :: atom() | binary(), Term :: term().
%% -----------------------------------------------------------------

ensure_undefined(Key, Term) ->
    case lookup(Key, Term) of
        false -> undefined;
        _ -> throw({format_error, {key_not_allowed, Key}})
    end.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

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
    throw({format_error, bad_structure}).


bin2atom(Key, {Key, Value}) when is_binary(Value) ->
    case erodlib:maybe_atom(Value, {}) of
        {} -> throw({format_error, {value_not_allowed, Key}});
        Atom -> Atom
    end;

bin2atom(Key, {Key, _Value}) ->
    throw({format_error, {bad_value_type, Key}});

bin2atom(Key, false) ->
    throw({format_error, {key_required, Key}}).


ensure_integer(Key, {Key, Value}) when is_integer(Value) -> Value;

ensure_integer(Key, {Key, _Value}) ->
    throw({format_error, {bad_value_type, Key}});

ensure_integer(Key, false) ->
    throw({format_error, {key_required, Key}}).


ensure_bin(Key, {Key, Value})
  when is_binary(Value) -> Value;

ensure_bin(Key, {Key, _Value}) ->
    throw({format_error, {bad_value_type, Key}});

ensure_bin(Key, false) ->
    throw({format_error, {key_required, Key}}).


ensure_bin(Key, {Key, Value}, _Default)
  when is_binary(Value) -> Value;

ensure_bin(Key, {Key, _Value}, _Default) ->
    throw({format_error, {bad_value_type, Key}});

ensure_bin(_Key, false, Default) ->
    Default.


ensure_struct(Key, {Key, Value}) ->
    Value;

ensure_struct(Key, false) ->
    throw({format_error, {key_required, Key}}).


ensure_struct(Key, {Key, Value}, _Default) ->
    Value;

ensure_struct(_Key, false, Default) ->
    Default.


ensure_bool(_Key, false, Default) -> Default;

ensure_bool(Key, {Key, Bool}, _Default) when is_boolean(Bool) -> Bool;

ensure_bool(Key, {Key, _Value}, _Default) ->
    throw({format_error, {bad_value_type, Key}}).


allowed(Key, Value, Allowed) ->
    case lists:member(Value, Allowed) of
        true -> Value;
        false -> throw({format_error, {value_not_allowed, Key}})
    end.


struct2key(Key, {Key, Term}) ->
    Type = get_atom(type, Term),
    Id = get_any(id, Term),
    build_key(Key, Type, Id);

struct2key(Key, false) ->
    throw({format_error, {key_required, Key}}).


struct2ver(Key, {Key, null}, Default) ->
    Default;

struct2ver(Key, {Key, List}, _Default) when is_list(List) ->
    [decode_ident_part(Key, P) || P <- List];

struct2ver(_Key, false, Default) ->
    Default.


struct2patch(Key, {Key, List}) when is_list(List) ->
    decode_patch(Key, List);

struct2patch(Key, {Key, _Other}) ->
    throw({format_error, {bad_value_type, Key}});

struct2patch(Key, false) ->
    throw({format_error, {key_required, Key}}).


build_key(_Key, Type, Num) when is_integer(Num) -> {Type, Num};

build_key(_Key, Type, Bin) when is_binary(Bin) -> {Type, Bin};

build_key(Key, Type, List) when is_list(List) ->
    {Type, [decode_ident_part(Key, P) || P <- List]};

build_key(Key, _Type, _List) ->
    throw({format_error, {bad_value_type, Key}}).


decode_ident_part(_Key, Value) when is_integer(Value) -> Value;

decode_ident_part(_Key, Value) when is_binary(Value) -> Value;

decode_ident_part(Key, _Value) ->
    throw({format_error, {bad_value_type, Key}}).


decode_patch(Key, List) ->
    [decode_patch_entry(Key, E) || E <- List].


decode_patch_entry(Key, Entry) ->
    decode_patch_entry(Key, get_atom(op, Entry), Entry).


decode_patch_entry(_Key, remove, Entry) ->
    Path = decode_patch_path(path, get_any(path, Entry)),
    {remove, Path};

decode_patch_entry(_Key, Op, Entry) when Op =:= add; Op =:= replace ->
    Path = decode_patch_path(path, get_any(path, Entry)),
    Value = get_any(value, Entry),
    {Op, Path, Value};

decode_patch_entry(_Key, Op, Entry) when Op =:= move; Op =:= copy ->
    From = decode_patch_path(from, get_any(path, Entry)),
    Path = decode_patch_path(path, get_any(path, Entry)),
    {Op, From, Path};

decode_patch_entry(Key, _Op, _Entry) ->
    throw({format_error, {value_not_allowed, Key}}).


decode_patch_path(_Key, Path) when is_binary(Path) ->
    case binary:split(Path, <<"/">>, [global]) of
        [<<>> |PathItems] ->
            [decode_patch_path_item(I) || I <- PathItems];
        _Any -> throw({format_error, {value_not_allowed}})
    end;

decode_patch_path(Key, _Path) ->
    throw({format_error, {bad_value_type, Key}}).


decode_patch_path_item(Item1) ->
    Item2 = binary:replace(Item1, <<"~1">>, <<"/">>, [global]),
    Item3 = binary:replace(Item2, <<"~0">>, <<"~">>, [global]),
    try binary_to_integer(Item3)
    catch error:badarg -> erodlib:maybe_atom(Item3, Item3)
    end.
