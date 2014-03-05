-ifndef(ERDOM_STORAGE_INCLUDED).
-define(ERDOM_STORAGE_INCLUDED, true).

-record(erdom_storage_group,
        {id,
         name}).

-record(erdom_storage_user,
        {id,
         username,
         password,
         first_name,
         last_name,
         display_name}).


-endif. % ERDOM_STORAGE_INCLUDED
