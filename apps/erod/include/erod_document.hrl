-ifndef(EROD_DOCUMENT_INCLUDED).
-define(EROD_DOCUMENT_INCLUDED, true).

-record(erod_content,
        {key :: erod:key(),
         ver :: erod:version() | undefined,
         type :: erod:content_type(),
         data :: erod:entity() | erod:patch()}).

-record(erod_page,
        {key :: erod:key(),
         ver :: erod:version(),
         view :: erod:view_id(),
         page :: erod:page_id(),
         size :: pos_integer(),
         total :: pos_integer(),
         type :: erod:content_type(),
         data :: erod:entity_items() | erod:patch()}).


-endif. % EROD_DOCUMENT_INCLUDED
