-module(erod_factory).

-include("erod_document.hrl").

 -export([new/2,
          knows_content/2,
          get_content/2,
          start_document/2,
          create_document/3]).

-define(Fac, ?MODULE).
-record(?Fac, {mod, sub}).


-callback init_factory(Options)
    -> {ok, State}
    when Options :: term(), State :: term().

-callback knows_content(DocKey, State)
    -> boolean()
    when DocKey :: erod:key(), State :: term().

-callback get_content(DocKey, State)
    -> {error, Reason}
     | {ok, Content}
    when DocKey :: erod:key(), State :: term(),
         Reason :: term(), Content :: term().

-callback start_document(DocKey, State)
    -> {ok, DocPid}
     | {error, Reason}
    when DocKey :: erod:key(), State :: term(),
         DocPid :: pid(), Reason :: term().

-callback create_document(DocKey, Options)
    -> {ok, Doc}
    when DocKey :: erod:key(), Options :: list(), Doc :: erod:document().


new(Module, Options) ->
    {ok, Sub} = Module:init_factory(Options),
    {ok, #?Fac{mod = Module, sub = Sub}}.


knows_content(DocKey, #?Fac{mod = Mod, sub = Sub}) ->
    Mod:knows_content(DocKey, Sub).


get_content(DocKey, #?Fac{mod = Mod, sub = Sub}) ->
    case Mod:get_content(DocKey, Sub) of
        {error, _Reason} = Error -> Error;
        {ok, Content} ->
            {ok, #erod_content{key = DocKey, type = entity, data = Content}}
    end.


start_document(DocKey, #?Fac{mod = Mod, sub = Sub}) ->
    Mod:start_document(DocKey, Sub).


create_document(DocKey, Module, Options) ->
    Module:create_document(DocKey, Options).
