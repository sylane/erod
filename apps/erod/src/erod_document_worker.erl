-module(erod_document_worker).

-behaviour(gen_server).

-include("erod_internal.hrl").

-export([start_link/3]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(St, ?MODULE).

-record(?St, {doc}).


start_link(DocKey, Factory, Options) ->
    gen_server:start_link(?MODULE, [DocKey, Factory, Options], []).


init([DocKey, Factory, Options]) ->
    lager:info("Starting document ~p worker...", [DocKey]),
    case Factory:create_document(DocKey, Options) of
        {error, Reason} -> {stop, Reason};
        {ok, Doc} -> {ok, #?St{doc = Doc}}
    end.


handle_call(Request, {From, _Ref}, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info(Info, #?St{doc = Doc} = State) ->
    case erod_document:handle_message(Info, Doc) of
        {ok, NewDoc} -> {noreply, State#?St{doc = NewDoc}};
        ignored ->
            lager:warning("Unexpected message: ~p", [Info]),
            {noreply, State}
    end.


terminate(Reason, #?St{doc = Doc}) ->
    lager:info("Terminating document ~p worker: ~p",
               [erod_document:key(Doc), Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
