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

-module (fiar_server_SUITE).
-author('euen@inakanetworks.com').

-type config() :: [{atom(), term()}].
-export([start/1, stop/1]).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([empty/1]).

-define(EXCLUDED_FUNS,
        [
         module_info,
         all,
         test,
         init_per_suite,
         end_per_suite,
         init_per_testcase,
         end_per_testcase,
         stop,
         empty
        ]).

%% @private
-spec all() -> [atom()].
all() ->
    Exports = ?MODULE:module_info(exports),
    [F || {F, _} <- Exports, not lists:member(F, ?EXCLUDED_FUNS)].

init_per_testcase(_, Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Match1 = fiar:start_match(<<"Brujo">>, <<"Euen">>),
  [{match1, Match1} | Config].

end_per_testcase(stop, Config) ->
  Config;
end_per_testcase(_, Config) ->
  fiar:stop(),
  Config.

-spec start(config()) -> ok.
start(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},

  Body = jiffy:encode(#{player1 => "Juan", player2 => "Fede"}),
  {ok, #{status_code := 200, body := _}} = api_call(post, "/matches", Headers, Body).

-spec stop(config()) -> ok.
stop(_Config) ->
  ok = fiar:stop(),
  timer:sleep(2000),
  %% [] = fiar_app(),
  ok.

%% @doc start returns an empty board
-spec empty(config()) -> ok.
empty(Config) ->
  Match1 = proplists:get_value(match1, Config),
  lists:foreach(
    fun(Col) ->
      next = fiar:play(Match1, Col),
      {next_player, _} = fiar:match_status(Match1)
    end,
    [1, 2, 3, 4, 5, 6, 7]),
  ok.

api_call(Method, Url, Headers, Body) ->
  {ok, Pid} = shotgun:open("localhost", 8080),
  Response = case Method of
                 post ->
                     shotgun:post(Pid, Url, Headers, Body, #{});
                 get ->
                     shotgun:get(Pid, Url, Headers, #{});
                 put ->
                     shotgun:put(Pid, Url, Headers, Body, #{})
             end,
  shotgun:close(Pid),
  Response.