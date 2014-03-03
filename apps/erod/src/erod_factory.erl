-module(erod_factory).

-include("erod_document.hrl").


-callback start_document(DocKey, Options)
    -> {ok, DocPid}
     | {error, document_not_found}
    when DocKey :: erod:key(), Options :: list(), DocPid :: pid().

-callback create_document(DocKey, Options)
    -> {ok, Doc}
    when DocKey :: erod:key(), Options :: list(), Doc :: erod:document().

