-module(erod).

-include("erod_document.hrl").
-include("erod_policy.hrl").

-export([start_document/3]).


-type key() :: erodlib:erod_key().
-type version() :: erodlib:erod_version().
-type patch() :: erodlib:erod_patch().

-type view_id() :: atom().
-type page_id() :: pos_integer().
-type user_id() :: pos_integer().
-type session_id() :: pos_integer().
-type session_token() :: binary().

-type content_type() :: entity | patch.
-type entity() :: tuple().
-type entity_item() :: {key(), entity()}.
-type entity_items() :: list(entity_item()) | [].

-type view_spec() :: {ViewId :: atom(),
                      PageSize :: pos_integer(),
                      OrderFun :: function()}.
-type view_specs() :: list(view_spec()) | [].

-type content() :: #erod_content{}.
-type page() :: #erod_page{}.
-type policy() :: #erod_policy{}.

-type document() :: erod_document:document().
-type context() :: erod_context:context().
-type proxy() :: erod_proxy:proxy().

-export_type([key/0, version/0, view_id/0, page_id/0,
              user_id/0, session_id/0, session_token/0,
              content_type/0, view_spec/0, view_specs/0,
              entity/0, entity_item/0, entity_items/0, patch/0,
              content/0, page/0, policy/0,
              document/0, context/0, proxy/0]).


start_document(DocKey, Factory, Options) ->
    erod_document_sup:start_child(DocKey, Factory, Options).
