% This file is licensed to you under the Apache License,
% Version 2.0 (the "License"); you may not use this file
% except in compliance with the License.  You may obtain
% a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing,
% software distributed under the License is distributed on an
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
% KIND, either express or implied.  See the License for the
% specific language governing permissions and limitations
% under the License.

%% @doc Third excercise: .
%%    == Required Knowledge ==
%%    <ul>
%%    <li>Modules and functions</li>
%%    <li>List functions</li>
%%    <li>Basic Erlang Structures</li>
%%    <li>Pattern Matching</li>
%%    <li>Gen Server</li>
%%    <li>Supervisor</li>
%%    <li>Application module</li>
%%    </ul>
%%
%%    == Problem Statement ==
%%    All calls into the system should go through the fiar module
%%    including but not limited to:
%%    <dl>
%%    <dt>start() -> ok.</dt>
%%    <dt>stop() -> ok.</dt>
%%    <dt>start_match() -> ok.</dt>
%%    <dt>stop_match() -> ok.</dt>
%%    </dl>

-module (fiar_SUITE).
-author('euen@inakanetworks.com').

-type config() :: [{atom(), term()}].
-export([all/0, start/1, stop/1, start_match/1,
         stop_match/1]).

init_per_testcase(stop, Config) ->
  {ok, Pid} = fiar:start(normal, []),
  [{pid, Pid} | Config].

%% @private
-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

-spec start(config()) -> ok.
start(_Config) ->
  ok = fiar:start(),
  [fiar] = fiar_app(),
  ok.

-spec stop(config()) -> ok.
stop(_Config) ->
  ok = fiar:stop(), timer:sleep(2000),
  [] = fiar_app(),
  ok.

-spec start_match(config()) -> ok.
start_match(_Config) ->
  fiar:start(normal, []),
  Pid = fiar:start_match(),
  case fiar:play(Pid, 1) of
    next -> ok;
    Error -> unexpected_error = Error 
  end.  

-spec stop_match(config()) -> ok.
stop_match(Config) ->
  Pid = proplists:get_value(pid, Config),
  ok = fiar:stop_match(Pid),
  try fiar:play(Pid, 1) of
    Result -> no_result = Result
  catch
    _:_ -> ok
  end.

fiar_app() ->
  [fiar || {fiar, _, _} <- application:which_applications()].