-module(erod).

-include("erod_document.hrl").

-export([start_document/3,
         start_websocket/1]).


-type key() :: {atom(), integer() | binary() | tuple()}.
-type version() :: term().
-type view_id() :: atom().
-type page_id() :: pos_integer().

-type content_type() :: entity | patch.
-type entity() :: tuple().
-type entity_item() :: {key(), entity()}.
-type entity_items() :: list(entity_item()) | [].
-type patch() :: list().

-opaque document() :: tuple().
-type content() :: #erod_content{}.
-type page() :: #erod_page{}.

-type view_spec() :: {atom(), module(), list()}.
-type view_specs() :: list(view_spec()).

-export_type([key/0, version/0, view_id/0, page_id/0,
              content_type/0, entity/0, entity_item/0, entity_items/0, patch/0,
              document/0, content/0, page/0,
              view_spec/0, view_specs/0]).


start_websocket(Port) when is_integer(Port) ->
    Dispatch = cowboy_router:compile(
                 [ {'_', [{'_', erod_connection, []}]} ]),
    cowboy:start_http(http_listener, 100, [{port, 8888}],
                      [{env, [{dispatch, Dispatch}]}]).


start_document(DocKey, Factory, Options) ->
    erod_document_sup:start_child(DocKey, Factory, Options).
