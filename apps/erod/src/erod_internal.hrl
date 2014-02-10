
-define(SESSION_SUP, session_supervisor).
-define(USER_SUP, user_supervisor).
-define(GROUP_SUP, group_supervisor).
-define(ROOM_SUP, room_supervisor).

-define(SESSION_MANAGER, session_manager).
-define(USER_MANAGER, user_manager).
-define(GROUP_MANAGER, group_manager).
-define(ROOM_MANAGER, room_manager).


-define(Ctx, context).
-record(context,
        {connection :: pid(),
         format :: atom()}).

-define(MsgKey, erod_message_key).
-record(erod_message_key,
        {type :: atom(),
         id :: term() | undefined}).

-define(Msg, erod_message).
-record(erod_message,
        {type :: request | result | error | notify,
         id :: binary() | undefiend,
         cls :: atom(),
         data :: term()}).

-define(MsgLogReq, erod_message_login_request).
-record(erod_message_login_request,
        {username :: binary(),
         password :: binary()}).

-define(MsgLogRes, erod_message_login_result).
-record(erod_message_login_result,
        {session :: binary(),
         self :: erod_message_key(),
         rooms :: erod_message_key(),
         fav :: erod_message_key(),
         root :: erod_message_key(),
         home :: erod_message_key(),
         config :: proplists:proplist()}).


-type erod_message_key() :: #erod_message_key{}.
