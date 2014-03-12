-module(erdom_document_user).

-behaviour(erod_document_factory).
-behaviour(erod_document).

-include("erdom_document.hrl").
-include("erdom_storage.hrl").

-export([init_factory/1,
         knows_content/2,
         get_content/2,
         start_document/2,
         create_document/2]).

-export([init/2,
         export_child_key/1,
         import_child_key/1]).

-define(St, ?MODULE).
-record(?St, {}).
-record(fac, {}).

-define(Content, erdom_document_user_content) .


init_factory([]) ->
    {ok, #fac{}}.


knows_content({user, _UserId}, _Fac) -> true;

knows_content(_DocKey, _Fac) -> false.


get_content(DocKey, _Fac) -> get_content(DocKey).


start_document({user, UserId} = DocKey, _Fac) ->
    case erdom_storage:does_user_exist(UserId) of
        true -> erod:start_document(DocKey, ?MODULE, []);
        false -> {error, document_not_found}
    end;

start_document(_DocKey, _Fac) ->
    {error, document_not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


init(DocKey, []) ->
    case get_content(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, Content} -> {ok, Content, [], [], #?St{}}
    end.


export_child_key(IntKey) -> IntKey.


import_child_key(ExtKey) -> ExtKey.


get_content({user, UserId}) ->
    case erdom_storage:get_user_content(UserId) of
        {error, user_not_found} -> {error, document_not_found};
        {error, _Reason} = Error -> Error;
        {ok, UserData} ->
            #erdom_storage_user{id = Id, first_name = FN,
                                last_name = LN, display_name = DN} = UserData,
            IdBin = integer_to_binary(Id),
            Pic = <<"user/", IdBin/bytes>>,
            {ok, #?Content{first_name = FN, last_name = LN,
                           display_name = DN, picture = Pic,
                           presence = offline, connected = false,
                           status = <<>>}}
    end.
