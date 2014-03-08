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
%%% @doc TODO: Document module erodlib_jsx.
%%% @end
%%% ==========================================================================

-module(erodlib_jsx).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% API functions
-export([decode/2,
         encode/2,
         encode_str/2, encode_str/3,
         encode_int/2,
         encode_bool/2,
         encode_any/2,
         encode_key/2,
         encode_ver/2,
         encode_patch/2]).


%%% ==========================================================================
%%% API Functions
%%% ==========================================================================

%% -----------------------------------------------------------------
%% @doc Decodes data in specified format to a JSX term structure.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec decode(Format, Data) -> Jsx
    when Format :: json, Data :: term(), Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

decode(json, Json) ->
    try jsx:decode(Json) of
%%TODO: Seems to fail dialyzer even though it is defined in decode/1 specs...
%%         {incomplete, _} -> error({format_error, incomplete_json});
        V when is_boolean(V); is_number(V); is_list(V); is_binary(V), V =:= null -> V;
        _ -> error({format_error, bad_json})
    catch
        error:badarg ->
            error({format_error, bad_json})
    end.


%% -----------------------------------------------------------------
%% @doc Encode a JSX term structure into specified format.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode(Format, Jsx) -> Data
    when Format :: json, Data :: term(), Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode(json, Jsx) ->
    try jsx:encode(Jsx)
    catch error:badarg -> error({format_error, bad_structure})
    end.


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated string value.
%%
%% Throw an error if the given value doesn't match the expectation.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_str(Key, Value) -> Jsx
    when Key :: atom(), Value :: binary() | atom() | term(),
         Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode_str(_Key, Bin) when is_binary(Bin) -> Bin;

encode_str(_Key, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);

encode_str(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated string value, if the value is part
%% of the given allowed values.
%%
%% Throw an error if the given value doesn't match the expectation.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_str(Key, Value, Allowed) -> Jsx
    when Key :: atom(), Value :: binary() | atom() | term(),
         Allowed :: [term()], Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode_str(Key, Value, Allowed) ->
    case lists:member(Value, Allowed) of
        true -> encode_str(Key, Value);
        false -> error({format_error, {value_not_allowed, Key}})
    end.


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated integer value.
%%
%% Throw an error if the given value doesn't match the expectation.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_int(Key, Value) -> Jsx
    when Key :: atom(), Value :: integer() | term(), Jsx :: jsx:json_term().
%% -----------------------------------------------------------------
encode_int(_Key, Value) when is_integer(Value) -> Value;

encode_int(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated boolean value.
%%
%% Throw an error if the given value doesn't match the expectation.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_bool(Key, Value) -> Jsx
    when Key :: atom(), Value :: boolean() | term(), Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode_bool(_Key, Value) when is_boolean(Value) -> Value;

encode_bool(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated term for the give value.
%%
%% Throw an error if the given value doesn't match any expectation.
%%
%% If the value is a record, assume there is a module with the same
%% name as the record with and encode/2 function able to encode it.
%%
%% If the value is a property list (a list with a tuple as the first element)
%% it will be converted by {@link erodlib_term:encode/2} function. If the
%% list is empty it will eb encoded as a list, not as an object.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_any(Key, Value) -> Jsx
    when Key :: atom(), Value :: term(), Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode_any(_Key, undefined) -> null;

encode_any(_Key, Bin) when is_binary(Bin) -> Bin;

encode_any(_Key, Bool) when is_boolean(Bool) -> Bool;

encode_any(_Key, Num) when is_integer(Num) -> Num;

encode_any(_Key, Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);

encode_any(Key, Rec) when is_tuple(Rec) ->
    try Rec:encode(jsx)
    catch error:undef ->
              lager:error("Encoding function ~p:encode(jsx, Data) not found "
                          "for data ~p", [element(1, Rec), Rec]),
              error({format_error, {unsuported_value, Key}})
    end;

encode_any(_Key, [T|_] = Term) when is_tuple(T) ->
    erodlib_term:encode(jsx, Term);

encode_any(Key, List) when is_list(List) ->
    [encode_any(Key, V) || V <- List];

encode_any(Key, _Other) ->
    error({format_error, {bad_value_type, Key}}).


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated term for an erod key.
%%
%% Throw an error if the given value doesn't match the expectation.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_key(Key, Value) -> Jsx
    when Key :: atom(), Value :: erodlib:erod_key() | undefined | term(),
         Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode_key(_Key, undefined) -> null;

encode_key(_Key, {Type, Num}) when is_integer(Num) ->
    [{<<"type">>, atom_to_binary(Type, utf8)}, {<<"id">>, Num}];

encode_key(_Key, {Type, Bin}) when is_binary(Bin) ->
    [{<<"type">>, atom_to_binary(Type, utf8)}, {<<"id">>, Bin}];

encode_key(Key, {Type, List}) when is_list(List) ->
    [{<<"type">>, atom_to_binary(Type, utf8)},
     {<<"id">>, [encode_ident_part(Key, V) || V <- List]}];

encode_key(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated term for an erod version.
%%
%% Throw an error if the given value doesn't match the expectation.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_ver(Key, Value) -> Jsx
    when Key :: atom(), Value :: erodlib:erod_version() | undefined | term(),
         Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode_ver(_Key, undefined) -> null;

encode_ver(Key, List) when is_list(List) ->
    [encode_ident_part(Key, V) || V <- List];

encode_ver(Key, _Value) ->
    error({format_error, {bad_value_type, Key}}).


%% -----------------------------------------------------------------
%% @doc Gives a JSX formated term for an erod patch.
%%
%% Throw an error if the given value doesn't match the expectation.
%%
%% The Key argument is the name of the attribute this value is for
%% to be able to add some context to the format error.
%% @throws {format_error, Reason}
%% @end
%% -----------------------------------------------------------------
-spec encode_patch(Key, Value) -> Jsx
    when Key :: atom(), Value :: erodlib:erod_patch() | term(),
         Jsx :: jsx:json_term().
%% -----------------------------------------------------------------

encode_patch(Key, Patch) ->
    encode_patch(Key, Patch, []).


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

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
             {<<"value">>, encode_any(Key, Value)}],
    encode_patch(Key, Rem, [Entry |Acc]);

encode_patch(Key, [{Op, From, Path} |Rem], Acc)
  when Op =:= move; Op =:= copy ->
    Entry = [{<<"op">>, atom_to_binary(Op, utf8)},
             {<<"from">>, encode_patch_path(Key, From)},
             {<<"path">>, encode_any(Key, Path)}],
    encode_patch(Key, Rem, [Entry |Acc]);

encode_patch(Key, _Any, _Acc) ->
    error({format_error, {bad_value_type, Key}}).


encode_patch_path(Key, Path) ->
    iolist_to_binary(patch_path_to_list(Key, Path)).


patch_path_to_list(_Key, []) -> [];

patch_path_to_list(Key, [Atom |Rem]) when is_atom(Atom) ->
    [$/, atom_to_list(Atom) |patch_path_to_list(Key, Rem)];

patch_path_to_list(Key, [Num |Rem]) when is_integer(Num) ->
    [$/, integer_to_list(Num) |patch_path_to_list(Key, Rem)];

patch_path_to_list(Key, _Path) ->
    error({format_error, {bad_value_type, Key}}).
