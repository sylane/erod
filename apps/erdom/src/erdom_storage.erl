-module(erdom_storage).

-behaviour(gen_server).

-include("erdom_internal.hrl").

-export([start_link/0]).

-export([get_index/0,
         get_group/1,
         get_user/1,
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


get_index() ->
    gen_server:call(?PROCESS, get_index).


get_group(GroupId) ->
    gen_server:call(?PROCESS, {get_group, GroupId}).


get_user(UserId) ->
    gen_server:call(?PROCESS, {get_user, UserId}).


does_group_exist(GroupId) ->
    gen_server:call(?PROCESS, {does_group_exist, GroupId}).


does_user_exist(UserId) ->
    gen_server:call(?PROCESS, {does_user_exist, UserId}).



init([]) ->
    lager:info("Starting erdom dummy storage...", []),
    {ok, AppName} = application:get_application(),
    PrivDir = code:priv_dir(AppName),
    UserDataFilename = filename:join([PrivDir, "users.dat"]),
    UserMap = load_users(UserDataFilename),
    GroupMap = create_groups(UserMap, 500),
    lager:info("Loaded ~p dummy users.", [gb_trees:size(UserMap)]),
    lager:info("Created ~p dummy groups.", [gb_trees:size(GroupMap)]),
    {ok, #?St{users = UserMap, groups = GroupMap}}.


handle_call({get_user, UserId}, _From, State) ->
    #?St{users = Users} = State,
    case gb_trees:lookup(UserId, Users) of
        {value, User} -> {reply, {ok, User}, State};
        none -> {reply, {error, not_found}, State}
    end;

handle_call({get_group, GroupId}, _From, State) ->
    #?St{groups = Groups} = State,
    case gb_trees:lookup(GroupId, Groups) of
        {value, Group} -> {reply, {ok, Group}, State};
        none -> {reply, {error, not_found}, State}
    end;

handle_call(get_index, _From, State) ->
    #?St{groups = Groups} = State,
    Index = #erdom_index{group_ids = gb_trees:keys(Groups)},
    {reply, {ok, Index}, State};

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
    Items = orddict:from_list([{U#erdom_user.id, U} || U <- Terms]),
    Map = gb_trees:from_orddict(Items),
    Map.



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
            Group = #erdom_group{id = GroupId, name = Name, user_ids = UserIds},
            NewGroupMap = gb_trees:insert(GroupId, Group, GroupMap),
            create_groups(Index, NewGroupMap, Count - 1)
    end.


pick_user_ids(Size, Index) ->
    pick_user_ids(Size, Index, gb_sets:new()).


pick_user_ids(0, _Index, Acc) -> gb_sets:to_list(Acc);

pick_user_ids(Count, Index, Acc) ->
    Idx = random:uniform(gb_trees:size(Index)),
    {value, User} = gb_trees:lookup(Idx, Index),
    UserId = User#erdom_user.id,
    case gb_sets:is_member(UserId, Acc) of
        true -> pick_user_ids(Count, Index, Acc);
        false -> pick_user_ids(Count - 1, Index, gb_sets:add(UserId, Acc))
    end.



