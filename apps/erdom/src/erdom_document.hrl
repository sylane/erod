-ifndef(ERDOM_DOCUMENT_INCLUDED).
-define(ERDOM_DOCUMENT_INCLUDED, true).

-include_lib("erod/include/erod_document.hrl").

-record(erdom_document_index_content,
        {}).

-record(erdom_document_index_child,
        {name}).

-record(erdom_document_group_content,
        {name}).

-record(erdom_document_group_child,
        {name,
         presence,
         connected}).

-record(erdom_document_user_content,
        {first_name,
         last_name,
         display_name,
         picture,
         presence,
         connected,
         status}).


-endif. % ERDOM_DOCUMENT_INCLUDED
