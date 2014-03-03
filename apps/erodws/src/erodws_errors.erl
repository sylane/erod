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

map(_, {format_error, Debug}) ->
    ?ERROR(40000, <<"Message Format Error">>, Debug);

map(login, {already_authenticated, Debug}) ->
    ?ERROR(40111, <<"Already Authenticated">>, Debug);

map(login, {unknown_user, Debug}) ->
    ?ERROR(40112, <<"Unknown User">>, Debug);

map(login, {invalid_credencial, Debug}) ->
    ?ERROR(40113, <<"Invalid Credential">>, Debug);

map(login, Debug) ->
    ?ERROR(40110, <<"Login Error">>, Debug);

map(reconnect, {already_authenticated, Debug}) ->
    ?ERROR(40121, <<"Already Authenticated">>, Debug);

map(reconnect, {unknown_session, Debug}) ->
    ?ERROR(40122, <<"Unknown Session">>, Debug);

map(reconnect, Debug) ->
    ?ERROR(40120, <<"Reconnect Error">>, Debug);

map(logout, {not_authenticated, Debug}) ->
    ?ERROR(40131, <<"Not Authenticated">>, Debug);

map(logout, Debug) ->
    ?ERROR(40130, <<"Logout Error">>, Debug);

map(get_content, {not_authenticated, Debug}) ->
    ?ERROR(40141, <<"Not Authenticated">>, Debug);

map(get_content, Debug) ->
    ?ERROR(60000, <<"Content Retrieval Error">>, Debug);

map(get_children, {not_authenticated, Debug}) ->
    ?ERROR(40151, <<"Not Authenticated">>, Debug);

map(get_children, Debug) ->
    ?ERROR(70000, <<"Children Retrieval Error">>, Debug);

map(_, {internal_error, Debug}) ->
    ?ERROR(50001, <<"Internal Server Error">>, Debug);

map(_, Debug) ->
    ?ERROR(50000, <<"Internal Server Error">>, Debug).


format_debug(undefined) -> undefined;

format_debug({undefined, undefined}) -> undefined;

format_debug({Something, undefined}) -> Something;

format_debug({Something, {undefined, undefined}}) -> Something;

format_debug({Some, {Thing, undefined}}) -> {Some, Thing};

format_debug(Something) -> Something.
