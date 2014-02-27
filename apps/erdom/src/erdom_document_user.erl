-module(erdom_document_user).

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

-define(Content, erdom_user_content) .


start_document({user, UserId} = DocKey, Options) ->
    case erdom_storage:does_user_exist(UserId) of
        true -> erod:start_document(DocKey, ?MODULE, Options);
        false -> {error, document_not_found}
    end;

start_document(_DocKey, []) ->
    {error, not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


init({user, UserId}, []) ->
    {Content, Children} = load_user(UserId),
    {ok, Content, Children, [], #?St{}}.


export_child_key(_IntKey) -> undefined.


import_child_key(_ExtKey) -> undefined.



load_user(UserId) ->
    {ok, UserData} = erdom_storage:get_user(UserId),
    Content = user_data_to_content(UserData),
    {Content, []}.

user_data_to_content(UD) ->
    #erdom_user{id = Id, first_name = FN,
                last_name = LN, display_name = DN} = UD,
    IdBin = integer_to_binary(Id),
    #erdom_user_content{first_name = FN, last_name = LN,
                        display_name = DN, picture = <<"user/", IdBin/bytes>>,
                        presence = offline, connected = false, status = <<>>}.
