-module(erodws_errors).

-include("erodws_protocol.hrl").

-export([map/2]).

-define(Err, erodws_proto_generic_error).
-define(ERROR(Code, Msg, Debug),
        #?Err{code = Code, msg = Msg, debug = format_debug(Debug)}).


map(Cls, A) when is_atom(A) ->
    map(Cls, {A, {undefined, undefined}});

map(Cls, {A, B}) when is_atom(A), is_atom(B) ->
    map(Cls, {A, {B, undefined}});

map(login, {format_error, Debug}) ->
    ?ERROR(400100, <<"Message Format Error">>, Debug);

map(login, {already_authenticated, Debug}) ->
    ?ERROR(401110, <<"Already Authenticated">>, Debug);

map(login, {unknown_user, Debug}) ->
    ?ERROR(401120, <<"User Not Found">>, Debug);

map(login, {invalid_credencial, Debug}) ->
    ?ERROR(401130, <<"Invalid Credential">>, Debug);

map(login, Debug) ->
    ?ERROR(500199, <<"Login Error">>, Debug);

map(reconnect, {format_error, Debug}) ->
    ?ERROR(400200, <<"Message Format Error">>, Debug);

map(reconnect, {already_authenticated, Debug}) ->
    ?ERROR(401210, <<"Already Authenticated">>, Debug);

map(reconnect, {unknown_session, Debug}) ->
    ?ERROR(401220, <<"Unknown Session">>, Debug);

map(reconnect, Debug) ->
    ?ERROR(500299, <<"Reconnect Error">>, Debug);

map(logout, {format_error, Debug}) ->
    ?ERROR(400300, <<"Message Format Error">>, Debug);

map(logout, {not_authenticated, Debug}) ->
    ?ERROR(401310, <<"Not Authenticated">>, Debug);

map(logout, Debug) ->
    ?ERROR(500399, <<"Logout Error">>, Debug);

map(get_content, {format_error, Debug}) ->
    ?ERROR(400400, <<"Message Format Error">>, Debug);

map(get_content, {not_authenticated, Debug}) ->
    ?ERROR(401410, <<"Not Authenticated">>, Debug);

map(get_content, {document_not_found, Debug}) ->
    ?ERROR(404420, <<"Document Not Found">>, Debug);

map(get_content, Debug) ->
    ?ERROR(600499, <<"Content Retrieval Error">>, Debug);

map(get_children, {format_error, Debug}) ->
    ?ERROR(400500, <<"Message Format Error">>, Debug);

map(get_children, {not_authenticated, Debug}) ->
    ?ERROR(401510, <<"Not Authenticated">>, Debug);

map(get_children, {document_not_found, Debug}) ->
    ?ERROR(404520, <<"Document Not Found">>, Debug);

map(get_children, {view_not_found, Debug}) ->
    ?ERROR(404530, <<"View Not Found">>, Debug);

map(get_children, {page_not_found, Debug}) ->
    ?ERROR(404540, <<"Page Not Found">>, Debug);

map(get_children, Debug) ->
    ?ERROR(500599, <<"Children Retrieval Error">>, Debug);

map(_, {format_error, Debug}) ->
    ?ERROR(400000, <<"Message Format Error">>, Debug);

map(_, {internal_error, Debug}) ->
    ?ERROR(500000, <<"Internal Server Error">>, Debug);

map(_, Debug) ->
    ?ERROR(500000, <<"Internal Server Error">>, Debug).


format_debug(undefined) -> undefined;

format_debug({undefined, undefined}) -> undefined;

format_debug({Something, undefined}) -> Something;

format_debug({Something, {undefined, undefined}}) -> Something;

format_debug({Some, {Thing, undefined}}) -> {Some, Thing};

format_debug(Something) -> Something.
