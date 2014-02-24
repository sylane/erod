-module(erod_document_sup).

-behaviour(supervisor).

-include("erod_internal.hrl").

-export([start_link/0]).

-export([start_child/3]).

-export([init/1]).

-define(PROCESS, ?MODULE).


start_link() ->
    supervisor:start_link({local, ?PROCESS}, ?MODULE, []).


start_child(DocKey, Factory, Options) ->
    supervisor:start_child(?PROCESS, [DocKey, Factory, Options]).


init([]) ->
    lager:info("Starting document supervisor...", []),
    {ok, { {simple_one_for_one, 5, 10},
          [{erod_document_worker, {erod_document_worker, start_link, []},
            transient, 5000, worker, [erod_document_worker]}]} }.

