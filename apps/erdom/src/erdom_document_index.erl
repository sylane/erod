-module(erdom_document_index).

-behaviour(erod_factory).
-behaviour(erod_document).

-include("erdom_document.hrl").
-include("erdom_storage.hrl").


-export([start_document/2,
         create_document/2]).

-export([init/2,
         export_child_key/1,
         import_child_key/1]).

-define(St, ?MODULE).
-record(?St, {}).

-define(GroupContent, erdom_document_group_content).
-define(Content, erdom_document_index_content).
-define(Child, erdom_document_index_child).


start_document({index, 0} = DocKey, Options) ->
    erod:start_document(DocKey, ?MODULE, Options);

start_document(_DocKey, []) ->
    {error, document_not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


init({index, 0}, []) ->
    {Content, Children} = load_index(),
    Views = [{asc, 50, fun compare_asc/2},
             {desc, 50, fun compare_desc/2}],
    {ok, Content, Children, Views, #?St{}}.


export_child_key(Key) -> {group, Key}.


import_child_key({group, Key}) -> Key.


compare_asc(#?Child{name = A}, #?Child{name = B}) -> A =< B.


compare_desc(#?Child{name = A}, #?Child{name = B}) -> A > B.


load_index() ->
    {ok, IndexData} = erdom_storage:get_index(),
    Content = index_data_to_content(IndexData),
    Children = [{GroupId, get_child_and_watch(GroupId)}
                || GroupId <- IndexData#erdom_storage_index.group_ids],
    {Content, Children}.


get_child_and_watch(GroupId) ->
    case erod_registry:find_content({group, GroupId}, undefined, self()) of
        {ok, #erod_content{type = entity, data = Content}} ->
            group_content_to_child(Content);
        {error, document_not_found} ->
            {ok, Data} = erdom_storage:get_group(GroupId),
            group_data_to_child(Data)
    end.


index_data_to_content(ID) ->
    #erdom_storage_index{} = ID,
    #?Content{}.


group_content_to_child(GC) ->
    #?GroupContent{name = N} = GC,
    #?Child{name = N}.


group_data_to_child(GD) ->
    #erdom_storage_group{name = N} = GD,
    #?Child{name = N}.

