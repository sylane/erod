-module(erod_factory).

-include("erod_document.hrl").


-callback start_document(DocKey :: erod:key(), Options :: list()) ->
    {ok, DocPid :: pid()}
  | {error, document_not_found}.


-callback create_document(DocKey :: erod:key(), Options :: list()) ->
    {ok, Doc :: erod:document()}.

