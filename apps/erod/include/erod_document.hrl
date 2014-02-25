-ifndef(EROD_DOCUMENT_INCLUDED).
-define(EROD_DOCUMENT_INCLUDED, true).

-record(erod_content,
        {key :: erod_key(),
         ver :: erod_version(),
         type :: erod_content_type(),
         data :: erod_entity() | erod_patch()}).

-record(erod_page,
        {key :: erod_key(),
         ver :: erod_version(),
         view :: erod_view_id(),
         page :: erod_page_id(),
         size :: pos_integer(),
         total :: pos_integer(),
         type :: erod_content_type(),
         data :: erod_entity_list() | erod_patch()}).


-type erod_key() :: {atom(), integer() | binary() | tuple()}.
-type erod_version() :: term().
-type erod_content_type() :: entity | patch.
-type erod_view_id() :: atom().
-type erod_page_id() :: pos_integer().

-type erod_entity() :: tuple().
-type erod_entity_list() :: list().
-type erod_patch() :: list().
-type erod_content() :: #erod_content{}.
-type erod_page() :: #erod_page{}.



-endif. % EROD_DOCUMENT_INCLUDED
