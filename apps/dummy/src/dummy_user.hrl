-ifndef(DUMMY_USER_INFO_INCLUDED).
-define(DUMMY_USER_INFO_INCLUDED, true).

-record(dummy_user_info,
        {token :: binary(),
         ref :: erod:key(),
         custom :: proplists:proplist()}).


-endif. % DUMMY_USER_INFO_INCLUDED
