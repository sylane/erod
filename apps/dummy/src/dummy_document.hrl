-ifndef(ERODWS_DUMMY_DOCUMENT_INCLUDED).
-define(ERODWS_DUMMY_DOCUMENT_INCLUDED, true).

-include_lib("erod/include/erod_document.hrl").

-record(dummy_document_content,
        {id :: integer(),
         name :: binary(),
		 label :: string(),
         flag :: boolean}).

-record(dummy_document_child,
        {pri :: high | low,
		 desc :: string()}).


-endif. % ERODWS_DUMMY_DOCUMENT_INCLUDED
