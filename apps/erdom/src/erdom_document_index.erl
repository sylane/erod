-module(erdom_document_index).

-include("erdom_internal.hrl").


-export([start_document/2,
         create_document/2]).

-export([init/3,
         export_child_key/1,
         import_child_key/1]).

-define(St, ?MODULE).
-record(?St, {}).

-define(Content, erdom_index_content) .
-define(Child, erdom_index_child) .


start_document({index, 0} = DocKey, Options) ->
    erod:start_document(DocKey, ?MODULE, Options);

start_document(_DocKey, []) ->
    {error, not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


init({index, 0}, [], Doc) ->
    {Content, Children} = load_index(),
    Views = [{asc, 50, fun compare_asc/2},
             {desc, 50, fun compare_desc/2}],
    {ok, Content, Children, Views, #?St{}, Doc}.


export_child_key(Key) -> {group, Key}.


import_child_key({group, Key}) -> Key.


compare_asc(#?Child{name = A}, #?Child{name = B}) -> A =< B.


compare_desc(#?Child{name = A}, #?Child{name = B}) -> A > B.


load_index() ->
    {ok, IndexData} = erdom_storage:get_index(),
    Content = index_data_to_content(IndexData),
    Children = [{GroupId, get_child_and_watch(GroupId)}
                || GroupId <- IndexData#erdom_index.group_ids],
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
    #erdom_index{} = ID,
    #erdom_index_content{}.


group_content_to_child(GC) ->
    #erdom_group_content{name = N} = GC,
    #erdom_index_child{name = N}.


group_data_to_child(GD) ->
    #erdom_group{name = N} = GD,
    #erdom_index_child{name = N}.

