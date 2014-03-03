-ifndef(ERDOM_USER_INCLUDED).
-define(ERDOM_USER_INCLUDED, true).

-record(erdom_proto_user_info,
        {session :: binary(),
         self :: erod:key(),
         rooms :: erod:key(),
         fav :: erod:key(),
         root :: erod:key(),
         home :: erod:key(),
         config :: proplists:proplist()}).



-endif. % ERDOM_USER_INCLUDED
