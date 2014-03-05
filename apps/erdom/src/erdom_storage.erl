-module(erdom_storage).

-behaviour(gen_server).

-include("erdom_storage.hrl").

-export([start_link/0]).

-export([get_index_children/0,
         get_group_content/1,
         get_group_children/1,
         get_user_content/1,
         get_user_by_username/1,
         does_group_exist/1,
         does_user_exist/1]).

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-define(PROCESS, ?MODULE).
-define(St, ?MODULE).

-record(?St, {users,
              groups}).



start_link() ->
    gen_server:start_link({local, ?PROCESS}, ?MODULE, [], []).


get_index_children() ->
    gen_server:call(?PROCESS, get_index_children).


get_group_content(GroupId) ->
    gen_server:call(?PROCESS, {get_group_content, GroupId}).

get_group_children(GroupId) ->
    gen_server:call(?PROCESS, {get_group_children, GroupId}).


get_user_content(UserId) ->
    gen_server:call(?PROCESS, {get_user_content, UserId}).


get_user_by_username(Username) ->
    gen_server:call(?PROCESS, {get_user_by_username, Username}).


does_group_exist(GroupId) ->
    gen_server:call(?PROCESS, {does_group_exist, GroupId}).


does_user_exist(UserId) ->
    gen_server:call(?PROCESS, {does_user_exist, UserId}).



init([]) ->
    lager:info("Starting erdom dummy storage...", []),
    _ = random:seed({10, 2, 77}),
    {ok, AppName} = application:get_application(),
    PrivDir = code:priv_dir(AppName),
    UserDataFilename = filename:join([PrivDir, "users.dat"]),
    UserMap = load_users(UserDataFilename),
    GroupMap = create_groups(UserMap, 500),
    lager:info("Loaded ~p dummy users.", [gb_trees:size(UserMap)]),
    lager:info("Created ~p dummy groups.", [gb_trees:size(GroupMap)]),
    {ok, #?St{users = UserMap, groups = GroupMap}}.


handle_call({get_user_content, UserId}, _From, State) ->
    #?St{users = Users} = State,
    case gb_trees:lookup(UserId, Users) of
        {value, User} -> {reply, {ok, User}, State};
        none -> {reply, {error, user_not_found}, State}
    end;

handle_call({get_user_by_username, Username}, _From, State) ->
    #?St{users = Users} = State,
    % As bad as it can be !
    List = gb_trees:to_list(Users),
    case [U || {_, U} <- List, U#erdom_storage_user.username =:= Username] of
        [User] -> {reply, {ok, User}, State};
        [] -> {reply, {error, user_not_found}, State}
    end;

handle_call({get_group_content, GroupId}, _From, State) ->
    #?St{groups = Groups} = State,
    case gb_trees:lookup(GroupId, Groups) of
        {value, {Group, _}} -> {reply, {ok, Group}, State};
        none -> {reply, {error, group_not_found}, State}
    end;

handle_call({get_group_children, GroupId}, _From, State) ->
    #?St{groups = Groups} = State,
    case gb_trees:lookup(GroupId, Groups) of
        {value, {_, UserIds}} -> {reply, {ok, UserIds}, State};
        none -> {reply, {error, group_not_found}, State}
    end;

handle_call(get_index_children, _From, State) ->
    #?St{groups = Groups} = State,
    {reply, {ok, gb_trees:keys(Groups)}, State};

handle_call({does_group_exist, GroupId}, _From, State) ->
    {reply, gb_trees:is_defined(GroupId, State#?St.groups), State};

handle_call({does_user_exist, UserId}, _From, State) ->
    {reply, gb_trees:is_defined(UserId, State#?St.users), State};

handle_call(Request, From, State) ->
    lager:error("Unexpected call from ~p: ~p", [From, Request]),
    {stop, {unexpected_call, Request, From}, {error, unexpected_call}, State}.


handle_cast(Request, State) ->
    lager:error("Unexpected cast: ~p", [Request]),
    {stop, {unexpected_cast, Request}, State}.


handle_info(Info, State) ->
    lager:warning("Unexpected message: ~p", [Info]),
    {noreply, State}.


terminate(Reason, _State) ->
    lager:debug("Terminating user manager: ~p", [Reason]),
    ok.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


load_users(Filename) ->
    {ok, Terms} = file:consult(Filename),
    Users = [{I, #erdom_storage_user{id = I, username = U, password = P,
                                     first_name = F, last_name = L,
                                     display_name = D}}
              || {I, U, P, F, L, D} <- Terms],
    gb_trees:from_orddict(orddict:from_list(Users)).



create_groups(UserMap, Count) ->
    Items = lists:zip(lists:seq(1, gb_trees:size(UserMap)),
                      gb_trees:values(UserMap)),
    FlatIndex = gb_trees:from_orddict(Items),
    create_groups(FlatIndex, gb_trees:empty(), Count).


create_groups(_Index, GroupMap, 0) -> GroupMap;

create_groups(Index, GroupMap, Count) ->
    GroupId = random:uniform(999999999),
    case gb_trees:lookup(GroupId, GroupMap) of
        {value, _} -> create_groups(Index, GroupMap, Count);
        none ->
            Number = gb_trees:size(GroupMap) + 1,
            Name = iolist_to_binary(io_lib:format("Group ~3w", [Number])),
            Size = random:uniform(400) + 200,
            UserIds = pick_user_ids(Size, Index),
            Group = #erdom_storage_group{id = GroupId, name = Name},
            NewGroupMap = gb_trees:insert(GroupId, {Group, UserIds}, GroupMap),
            create_groups(Index, NewGroupMap, Count - 1)
    end.


pick_user_ids(Size, Index) ->
    pick_user_ids(Size, Index, gb_sets:new()).


pick_user_ids(0, _Index, Acc) -> gb_sets:to_list(Acc);

pick_user_ids(Count, Index, Acc) ->
    Idx = random:uniform(gb_trees:size(Index)),
    {value, User} = gb_trees:lookup(Idx, Index),
    UserId = User#erdom_storage_user.id,
    case gb_sets:is_member(UserId, Acc) of
        true -> pick_user_ids(Count, Index, Acc);
        false -> pick_user_ids(Count - 1, Index, gb_sets:add(UserId, Acc))
    end.



