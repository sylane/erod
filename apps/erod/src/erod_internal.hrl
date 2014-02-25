
-include("erod_document.hrl").


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
         self :: erod_key(),
         rooms :: erod_key(),
         fav :: erod_key(),
         root :: erod_key(),
         home :: erod_key(),
         config :: proplists:proplist()}).

-define(MsgRecReq, erod_message_reconnect_request).
-record(erod_message_reconnect_request,
        {session :: binary()}).

-define(MsgGetConReq, erod_message_get_content_request).
-record(erod_message_get_content_request,
        {key :: erod_key(),
         ver :: erod_version(),
         subscribe :: boolean()}).

-define(MsgGetConRes, erod_message_get_content_result).
-record(erod_message_get_content_result,
        {content :: erod_content()}).

-define(MsgGetChiReq, erod_message_get_children_request).
-record(erod_message_get_children_request,
        {key :: erod_key(),
         ver :: erod_version(),
         view :: erod_view_id(),
         page :: erod_page_id(),
         subscribe :: boolean()}).

-define(MsgGetChiRes, erod_message_get_children_result).
-record(erod_message_get_children_result,
        {page :: erod_page()}).


-type erod_user_identity() :: #erod_user_identity{}.
-type erod_session_policy() :: #erod_session_policy{}.
-type erod_context() :: #erod_context{}.
