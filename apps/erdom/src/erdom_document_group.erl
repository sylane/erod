-module(erdom_document_group).

-behaviour(erod_factory).
-behaviour(erod_document).

-include("erdom_internal.hrl").

-export([start_document/2,
         create_document/2]).

-export([init/2,
         export_child_key/1,
         import_child_key/1]).

-define(St, ?MODULE).
-record(?St, {}).

-define(Content, erdom_group_content) .
-define(Child, erdom_group_child) .


start_document({group, GroupId} = DocKey, Options) ->
    case erdom_storage:does_group_exist(GroupId) of
        true -> erod:start_document(DocKey, ?MODULE, Options);
        false -> {error, document_not_found}
    end;

start_document(_DocKey, []) ->
    {error, not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


init({group, GroupId}, []) ->
    {Content, Children} = load_group(GroupId),
    Views = [{asc, 50, fun compare_asc/2},
             {desc, 50, fun compare_desc/2},
             {asc_pres_first, 50, fun compare_asc_pres_first/2},
             {desc_pres_first, 50, fun compare_desc_pres_first/2}],
    {ok, Content, Children, Views, #?St{}}.


export_child_key(Key) -> {user, Key}.


import_child_key({user, Key}) -> Key.



compare_asc(#?Child{name = A}, #?Child{name = B}) -> A =< B.


compare_desc(#?Child{name = A}, #?Child{name = B}) -> A > B.


compare_asc_pres_first(#?Child{name = A, presence = offline},
                       #?Child{name = B, presence = offline}) -> A =< B;

compare_asc_pres_first(#?Child{}, #?Child{presence = offline}) -> true;

compare_asc_pres_first(#?Child{presence = offline}, #?Child{}) -> false;

compare_asc_pres_first(#?Child{name = A}, #?Child{name = B}) -> A =< B.


compare_desc_pres_first(#?Child{name = A, presence = offline},
                        #?Child{name = B, presence = offline}) -> A > B;

compare_desc_pres_first(#?Child{}, #?Child{presence = offline}) -> true;

compare_desc_pres_first(#?Child{presence = offline}, #?Child{}) -> false;

compare_desc_pres_first(#?Child{name = A}, #?Child{name = B}) -> A > B.



load_group(GroupId) ->
    {ok, GroupData} = erdom_storage:get_group(GroupId),
    Content = group_data_to_content(GroupData),
    Children = [{UserId, get_child_and_watch(UserId)}
                || UserId <- GroupData#erdom_group.user_ids],
    {Content, Children}.


get_child_and_watch(UserId) ->
    case erod_registry:find_content({user, UserId}, undefined, self()) of
        {ok, #erod_content{type = entity, data = Content}} ->
            user_content_to_child(Content);
        {error, document_not_found} ->
            {ok, Data} = erdom_storage:get_user(UserId),
            user_data_to_child(Data)
    end.


group_data_to_content(GD) ->
    #erdom_group{name = N} = GD,
    #erdom_group_content{name = N}.


user_content_to_child(UC) ->
    #erdom_user_content{display_name = N, presence = P, connected = C} = UC,
    #erdom_group_child{name = N, presence = P, connected = C}.


user_data_to_child(UD) ->
    #erdom_user{display_name = N} = UD,
    #erdom_group_child{name = N, presence = offline, connected = false}.

