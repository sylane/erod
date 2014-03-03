-module(erdom_user).

-behaviour(erod_user).

-include_lib("erod/include/erod_policy.hrl").

-include("erdom_storage.hrl").
-include("erdom_user.hrl").

-export([init/2,
         info/2,
         authenticate/2]).

-define(St, ?MODULE).
-record(?St, {id :: pos_integer(),
              user :: binary(),
              pass :: binary()}).


init(Username, []) ->
    case erdom_storage:get_user_by_username(Username) of
        {error, not_found} -> {stop, unknown_user};
        {ok, UserInfo} ->
            #erdom_storage_user{id = I, username = U, password = P} = UserInfo,
            {ok, I, #?St{id = I, user = U, pass = P}}
    end.


info(SessionToken, #?St{id = UserId} = State) ->
    Config = [{media_server, <<"http://foo.bar.com/statics">>}],
    {ok, #erdom_proto_user_info{session = SessionToken,
                                self = {user, UserId},
                                rooms = {rooms, UserId},
                                fav = {fav, UserId},
                                root = {index, 0},
                                home = undefined,
                                config = Config}, State}.


authenticate({User, Pass}, #?St{user = User, pass = Pass} = State) ->
    {ok, #erod_policy{}, State};

authenticate({User, _}, #?St{user = User} = State) ->
    {error, invalid_credencial, State};

authenticate({User1, _}, #?St{user = User2} = State) ->
    lager:error("User ~p received an authentication request for user ~p",
                [User2, User1]),
    {error, invalid_credencial, State}.
