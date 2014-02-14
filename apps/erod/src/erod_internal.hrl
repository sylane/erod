
-define(SESSION_SUP, session_supervisor).
-define(USER_SUP, user_supervisor).
-define(GROUP_SUP, group_supervisor).
-define(ROOM_SUP, room_supervisor).

-define(SESSION_MANAGER, session_manager).
-define(USER_MANAGER, user_manager).
-define(GROUP_MANAGER, group_manager).
-define(ROOM_MANAGER, room_manager).


-define(UserIdent, erod_user_identity).
-record(erod_user_identity,
        {username :: binary()}).

-define(UserCred, erod_user_credential).
-record(erod_user_credential,
        {identity :: erod_user_identity(),
         password :: binary()}).

-define(SessPol, erod_session_policy).
-record(erod_session_policy,
        {user_id :: pos_integer()}).

-define(Ctx, erod_context).
-record(erod_context,
        {fmt :: atom(),
         conn :: pid(),
         sess :: pid() | undefined,
         user :: pid() | undefined,
         policy :: #erod_session_policy{} | undefined}).

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

-define(MsgRecReq, erod_message_reconnect_request).
-record(erod_message_reconnect_request,
        {session :: binary()}).


-type erod_message_key() :: #erod_message_key{}.
-type erod_user_identity() :: #erod_user_identity{}.
-type erod_session_policy() :: #erod_session_policy{}.
-type erod_context() :: #erod_context{}.
