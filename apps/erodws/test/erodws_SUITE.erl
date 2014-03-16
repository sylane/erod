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
%%% Includes
%%% ==========================================================================

-export([garbage_packet/1]).

%%% Common test functions
-export([all/0,
         suite/0,
		 init_per_suite/1,
		 init_per_testcase/2,
         end_per_testcase/2,
		 end_per_suite/1]).


%%% ==========================================================================
%%% common_test callbacks
%%% ==========================================================================

all() -> [garbage_packet].


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

garbage_packet(_Conf) ->
	ok.


%% ====================================================================
%% Internal functions
%% ====================================================================



wait_terminated(Pid) ->
    Ref = monitor(process, Pid),
    receive
        {'DOWN', Ref, process, Pid, Reason} -> Reason
    after 2000 -> ct:fail("Process ~w not terminated", [Pid])
    end.
