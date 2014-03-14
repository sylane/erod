%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erdom.
%%%
%%% Erdom is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <http://www.gnu.org/licenses/>.
%%% ==========================================================================
%%% @copyright 2014 Sebastien Merle <s.merle@gmail.com>
%%% @author Sebastien Merle <s.merle@gmail.com>
%%% @doc TODO: Document module erdom_document_user.
%%% @end
%%% ==========================================================================

-module(erdom_document_user).

-author('Sebastien Merle').

-behaviour(erod_document_factory).
-behaviour(erod_document).


%%% ==========================================================================
%%% Includes
%%% ==========================================================================

-include("erdom_document.hrl").
-include("erdom_storage.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

%%% Behaviour erod_document_factory callbacks
-export([init_factory/1,
         knows_content/2,
         get_content/2,
         start_document/2,
         create_document/2]).

%%% Behaviour erod_document callbacks
-export([init/2,
         export_child_key/1,
         import_child_key/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(St, ?MODULE).
-define(Content, erdom_document_user_content) .


%%% ==========================================================================
%%% Records
%%% ==========================================================================

-record(?St, {}).
-record(fac, {}).


%%% ==========================================================================
%%% Behaviour erod_document_factory Callbacks
%%% ==========================================================================

init_factory([]) ->
    {ok, #fac{}}.


knows_content({user, _UserId}, _Fac) -> true;

knows_content(_DocKey, _Fac) -> false.


get_content(DocKey, _Fac) -> get_content(DocKey).


start_document({user, UserId} = DocKey, _Fac) ->
    case erdom_storage:does_user_exist(UserId) of
        true -> erod:start_document(DocKey, ?MODULE, []);
        false -> {error, document_not_found}
    end;

start_document(_DocKey, _Fac) ->
    {error, document_not_found}.


create_document(DocKey, Options) ->
    {ok, erod_document:new(DocKey, ?MODULE, Options)}.


%%% ==========================================================================
%%% Behaviour erod_document Callbacks
%%% ==========================================================================

init(DocKey, []) ->
    case get_content(DocKey) of
        {error, _Reason} = Error -> Error;
        {ok, Content} -> {ok, Content, [], [], #?St{}}
    end.


export_child_key(IntKey) -> IntKey.


import_child_key(ExtKey) -> ExtKey.


%%% ==========================================================================
%%% Internal Functions
%%% ==========================================================================

get_content({user, UserId}) ->
    case erdom_storage:get_user_content(UserId) of
        {error, user_not_found} -> {error, document_not_found};
        {error, _Reason} = Error -> Error;
        {ok, UserData} ->
            #erdom_storage_user{id = Id, first_name = FN,
                                last_name = LN, display_name = DN} = UserData,
            IdBin = integer_to_binary(Id),
            Pic = <<"user/", IdBin/bytes>>,
            {ok, #?Content{first_name = FN, last_name = LN,
                           display_name = DN, picture = Pic,
                           presence = offline, connected = false,
                           status = <<>>}}
    end.
