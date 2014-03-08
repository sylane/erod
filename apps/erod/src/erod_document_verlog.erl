-module(erod_document_verlog).

-export([new/0,
         version/1,
         add_patch/2,
         add_patch/3,
         commit/1,
         get_patch/2]).

-define(VerLog, ?MODULE).
-record(?VerLog, {identity,
                  version,
                  current,
                  history}).



new() ->
    Identity = crypto:rand_uniform(10000000,100000000),
    #?VerLog{identity = Identity, version = 0,
             current = [], history = erodlib_maps:new()}.


version(#?VerLog{identity = Identity, version = Version}) ->
    {Identity, Version}.


add_patch(Entry, #?VerLog{current = Current} = VerLog) when is_tuple(Entry) ->
    VerLog#?VerLog{current = [Entry |Current]};

add_patch(Patch, #?VerLog{current = Current} = VerLog) when is_list(Patch) ->
    VerLog#?VerLog{current = lists:reverse(Patch, Current)}.


add_patch(Prefix, Patch, #?VerLog{current = Current} = VerLog) when is_list(Patch) ->
    Prefixed = [add_prefix(Prefix, Entry) || Entry <- Patch],
    VerLog#?VerLog{current = lists:reverse(Prefixed, Current)}.


commit(#?VerLog{current = []} = VerLog) -> {false, VerLog};

commit(#?VerLog{version = Version, current = Current, history = History} = VerLog) ->
    NewHistory = erodlib_maps:insert(Version, lists:reverse(Current), History),
    {true, VerLog#?VerLog{version = Version + 1,
                          current = [], history = NewHistory}}.


get_patch(undefined, _VerLog) -> none;

get_patch({Identity, Ver}, #?VerLog{identity = Identity, version = Ver}) ->
    unchanged;

get_patch({Identity, FromVer}, #?VerLog{identity = Identity} = VerLog) ->
    #?VerLog{version = Version, history = History} = VerLog,
    case erodlib_maps:lookup_from(FromVer, History) of
        {values, Patches} -> {patch, Version, lists:flatten(Patches)};
        none -> none
    end;

get_patch(_FromVer, _VerLog) -> none.


add_prefix(Prefix, {remove, Path}) ->
    {remove, [Prefix| Path]};

add_prefix(Prefix, {Op, Path, Value})
  when Op =:= add; Op =:= replace ->
    {Op, [Prefix| Path], Value};

add_prefix(Prefix, {Op, Path1, Path2})
  when Op =:= move; Op =:= copy ->
    {Op, [Prefix| Path1], [Prefix| Path2]}.
