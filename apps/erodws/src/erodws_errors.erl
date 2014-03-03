-module(erodws_errors).

-include("erodws_protocol.hrl").

-export([map/1]).

-define(Err, erodws_proto_generic_error).
-define(ERROR(Code, Msg, Debug),
        #?Err{code = Code, msg = Msg, debug = format_debug(Debug)}).


map(A) when is_atom(A) ->
    map({A, {undefined, undefined}});

map({A, B}) when is_atom(A), is_atom(B) ->
    map({A, {B, undefined}});

map({format_error, Debug}) ->
    ?ERROR(40000, <<"Message Format Error">>, Debug);

map({login_error, {already_authenticated, Debug}}) ->
    ?ERROR(40111, <<"Already Authenticated">>, Debug);

map({login_error, {unknown_user, Debug}}) ->
    ?ERROR(40112, <<"Unknown User">>, Debug);

map({login_error, {invalid_credencial, Debug}}) ->
    ?ERROR(40113, <<"Invalid Credential">>, Debug);

map({login_error, Debug}) ->
    ?ERROR(40110, <<"Login Error">>, Debug);

map({restore_error, {already_authenticated, Debug}}) ->
    ?ERROR(40121, <<"Already Authenticated">>, Debug);

map({restore_error, {unknown_session, Debug}}) ->
    ?ERROR(40122, <<"Unknown Session">>, Debug);

map({restore_error, Debug}) ->
    ?ERROR(40120, <<"Reconnect Error">>, Debug);

map({logout_error, {not_authenticated, Debug}}) ->
    ?ERROR(40131, <<"Not Authenticated">>, Debug);

map({logout_error, Debug}) ->
    ?ERROR(40130, <<"Logout Error">>, Debug);

map({get_content_error, {not_authenticated, Debug}}) ->
    ?ERROR(40141, <<"Not Authenticated">>, Debug);

map({get_content_error, Debug}) ->
    ?ERROR(60000, <<"Content Retrieval Error">>, Debug);

map({get_children_error, {not_authenticated, Debug}}) ->
    ?ERROR(40151, <<"Not Authenticated">>, Debug);

map({get_children_error, Debug}) ->
    ?ERROR(70000, <<"Children Retrieval Error">>, Debug);

map({internal_error, Debug}) ->
    ?ERROR(50001, <<"Internal Server Error">>, Debug);

map(Debug) ->
    ?ERROR(50000, <<"Internal Server Error">>, Debug).


format_debug(undefined) -> undefined;

format_debug({undefined, undefined}) -> undefined;

format_debug({Something, undefined}) -> Something;

format_debug({Something, {undefined, undefined}}) -> Something;

format_debug({Some, {Thing, undefined}}) -> {Some, Thing};

format_debug(Something) -> Something.
