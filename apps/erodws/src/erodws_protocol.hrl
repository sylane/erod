-ifndef(ERODWS_PROTOCOL_INCLUDED).
-define(ERODWS_PROTOCOL_INCLUDED, true).

-define(Msg, erodws_proto_message).

-record(erodws_proto_message,
        {type :: request | result | error | notify,
         id :: erodws_request_id() | undefiend,
         cls :: atom(),
         status :: integer() | undefined,
         data :: term()}).

-record(erodws_proto_get_content_request,
        {key :: erod:key(),
         ver :: erod:version(),
         subscribe :: boolean()}).

-record(erodws_proto_get_children_request,
        {key :: erod:key(),
         ver :: erod:version(),
         view :: erod:view_id(),
         page :: erod:page_id(),
         subscribe :: boolean()}).

-record(erodws_proto_patch_content_request,
        {key :: erod:key(),
         ver :: erod:version(),
         format :: atom(),
         patch :: erod:patch()}).

-record(erodws_proto_generic_error,
        {status :: pos_integer(),
         code :: pos_integer(),
         msg :: binary() | undefined,
         debug :: binary() | undefined}).


-type erodws_message() :: #erodws_proto_message{}.
-type erodws_error() :: #erodws_proto_generic_error{}.
-type erodws_request_id() :: binary().


-endif. % ERODWS_PROTOCOL_INCLUDED
