%%% ==========================================================================
%%% Copyright (c) 2014 Sebastien Merle <s.merle@gmail.com>
%%%
%%% This file is part of erodws.
%%%
%%% Erodws is free software: you can redistribute it and/or modify
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

-module(erodws_SUITE).

-author('Sebastien Merle').


%%% ==========================================================================
%%% Includes
%%% ==========================================================================


-include_lib("common_test/include/ct.hrl").
-include_lib("eunit/include/eunit.hrl").


%%% ==========================================================================
%%% Exports
%%% ==========================================================================

-export([garbage_packet/1,
		 incomplete_packet/1,
		 invalid_packet_type/1,
		 request_without_id/1]).

%%% Common test functions
-export([all/0,
         suite/0,
		 init_per_suite/1,
		 init_per_testcase/2,
         end_per_testcase/2,
		 end_per_suite/1]).


%%% ==========================================================================
%%% Macros
%%% ==========================================================================

-define(TIMEOUT, 2000).

-define(assertClosed(Pid),
		receive
			{closed, Pid} -> ok
	    after ?TIMEOUT ->
			ct:fail("Connection not closed")
        end).


%%% ==========================================================================
%%% common_test callbacks
%%% ==========================================================================

all() -> [garbage_packet,
		  incomplete_packet,
		  invalid_packet_type,
		  request_without_id].


suite() -> [{timetrap, {seconds, 10}}].


init_per_suite(Config) ->
	{ok, _} = application:ensure_all_started(dummy),
	Config.


init_per_testcase(_TestCase, Conf) ->
	URI = "ws://localhost:8888",
	{ok, Client} = dummy_client:start_link(URI, self()),
    [{client, Client} |Conf].


end_per_testcase(_TestCase, Conf) ->
	Client = ?config(client, Conf),
	dummy_client:close(Client),
	wait_terminated(Client),
	ok.


end_per_suite(_Config) ->
	ok = application:stop(dummy),
	ok = application:stop(erodws),
    ok = application:stop(erod),
    ok.


%%% ==========================================================================
%%% Tests
%%% ==========================================================================

garbage_packet(Conf) ->
	Cli = ?config(client, Conf),
	dummy_client:send(Cli, <<"!@#$">>),
	?assertClosed(Cli),
	ok.


incomplete_packet(Conf) ->
	Cli = ?config(client, Conf),
	dummy_client:send(Cli, <<"{\"request\":">>),
	?assertClosed(Cli),
	ok.


invalid_packet_type(Conf) ->
	Cli = ?config(client, Conf),
	dummy_client:send(Cli, <<"{\"type\":\"spam\",\"cls\":\"login\"}">>),
	?assertClosed(Cli),
	ok.


request_without_id(Conf) ->
	Cli = ?config(client, Conf),
	dummy_client:send(Cli, <<"{\"type\":\"request\",\"cls\":\"login\"}">>),
	?assertClosed(Cli),
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================



wait_terminated(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} -> Reason
    after ?TIMEOUT -> ct:fail("Process ~w not terminated", [Pid])
    end.
