-module(erod_document_verlog).

-export([new/0,
         version/1,
         add_patch/2,
         add_patch/3,
         commit/1,
         get_patch/2]).

-define(VerLog, ?MODULE).
-record(?VerLog, {version,
                  current,
                  patches}).



new() ->
    #?VerLog{version = 0, current = [], patches = erod_maps:new()}.


version(#?VerLog{version = Version}) -> Version.


add_patch(Entry, #?VerLog{current = Current} = VerLog) when is_tuple(Entry) ->
    VerLog#?VerLog{current = [Entry |Current]};

add_patch(Patch, #?VerLog{current = Current} = VerLog) when is_list(Patch) ->
    VerLog#?VerLog{current = lists:reverse(Patch, Current)}.


add_patch(Prefix, Patch, #?VerLog{current = Current} = VerLog) when is_list(Patch) ->
    Prefixed = [add_prefix(Prefix, Entry) || Entry <- Patch],
    VerLog#?VerLog{current = lists:reverse(Prefixed, Current)}.


commit(#?VerLog{version = Version, current = Current, patches = Patches} = VerLog) ->
    NewPatches = erod_maps:insert(Version, lists:reverse(Current), Patches),
    VerLog#?VerLog{version = Version + 1, current = [], patches = NewPatches}.


get_patch(FromVer, #?VerLog{patches = Patches}) ->
    case erod_map:lookup_from(FromVer, Patches) of
        {values, Patches} -> {patch, lists:flatten(Patches)};
        none -> none
    end.



add_prefix(Prefix, {remove, Path}) ->
    {remove, [Prefix| Path]};

add_prefix(Prefix, {Op, Path, Value})
  when Op =:= add; Op =:= replace ->
    {Op, [Prefix| Path], Value};

add_prefix(Prefix, {Op, Path1, Path2})
  when Op =:= move; Op =:= copy ->
    {Op, [Prefix| Path1], [Prefix| Path2]}.
