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
-export([start/1]).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([ get_matches/1
        , get_status/1
        , first_play/1
        , play_bad_id/1
        , wins_vertically/1
        , wins_horizontally/1
        , wins_right_diagonally/1
        , wins_left_diagonally/1
        ]).

%% @private
-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

init_per_testcase(get_matches, Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Config;
init_per_testcase(get_status, Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Config;
init_per_testcase(first_play, Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Config;
init_per_testcase(play_bad_id, Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Config;
init_per_testcase(_, Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Headers = #{<<"content-type">> => <<"application/json">>},
  PlayersBody = jiffy:encode(#{player1 => "Juan", player2 => "Fede"}),
  {ok, #{status_code := 200, body := NewBody}} =
    api_call(post, "/matches", Headers, PlayersBody),
  BodyDecode = jiffy:decode(NewBody, [return_maps]),
  Mid = integer_to_list(maps:get(<<"id">>, BodyDecode)),
  [{mid, Mid}, {headers, Headers}| Config].


end_per_testcase(_, Config) ->
  fiar:stop(),
  Config.

-spec start(config()) -> ok.
start(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  Body = jiffy:encode(#{player1 => "Juan", player2 => "Fede"}),
  {ok, #{status_code := 200, body := _}} =
    api_call(post, "/matches", Headers, Body).

-spec get_matches(config()) -> ok.
get_matches(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  {ok, #{status_code := 200, body := RespBody}} =
    api_call(get, "/matches", Headers),
  true = is_list(jiffy:decode(RespBody)).

get_status(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  PlayersBody = jiffy:encode(#{player1 => "Juan", player2 => "Fede"}),
  {ok, #{status_code := 200, body := NewBody}} =
    api_call(post, "/matches", Headers, PlayersBody),
  Mid =
    integer_to_list(maps:get(<<"id">>, jiffy:decode(NewBody, [return_maps]))),
  {ok, #{status_code := 200, body := RespBody}} =
    api_call(get, "/matches/"++Mid, Headers, #{}),
  <<"on_course">> =
    maps:get(<<"status">>, jiffy:decode(RespBody, [return_maps])),
  ok.

first_play(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  PlayersBody = jiffy:encode(#{player1 => "Juan", player2 => "Fede"}),
  {ok, #{status_code := 200, body := NewBody}} =
    api_call(post, "/matches", Headers, PlayersBody),
  BodyDecode = jiffy:decode(NewBody, [return_maps]),
  Mid = integer_to_list(maps:get(<<"id">>, BodyDecode)),
  MoveBody = jiffy:encode(#{column => 1}),
  {ok, #{status_code := 200, body := RespBody}} = 
    api_call(put, "/matches/" ++ Mid, Headers, MoveBody),
  State = maps:get(<<"state">>, jiffy:decode(RespBody, [return_maps])),
  [[1]|_] = maps:get(<<"board">>, State),
  2 = maps:get(<<"next_chip">>, State),
  ok.

play_bad_id(_config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  PlayersBody = jiffy:encode(#{player1 => "Juan", player2 => "Fede"}),
  api_call(post, "/matches", Headers, PlayersBody),
  MoveBody = jiffy:encode(#{column => 1}),
  {ok, #{status_code := 400}} = 
    api_call(put, "/matches/id", Headers, MoveBody),
  {ok, #{status_code := 404}} = 
    api_call(put, "/matches/2056356", Headers, MoveBody),
  ok.

%% @doc when the player puts 4 chips in a vertical row, wins
-spec wins_vertically(config()) -> ok.
wins_vertically(Config) ->
  Mid = proplists:get_value(mid, Config),
  Headers = proplists:get_value(headers, Config),
  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([1, 2, 1, 2, 1, 2, 1], Mid, Headers),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  State = maps:get(<<"state">>, BodyDecode),
  [[1, 1, 1], [2, 2, 2], [], [], [], [], []] = maps:get(<<"board">>, State),
  1 = maps:get(<<"next_chip">>, State),
  <<"won_by_player1">> = maps:get(<<"status">>, BodyDecode),
  ok.

%% @doc when the player puts 4 chips in a horizontal row, wins
-spec wins_horizontally(config()) -> ok.
wins_horizontally(Config) ->
  Mid = proplists:get_value(mid, Config),
  Headers = proplists:get_value(headers, Config),
  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([2, 5, 3, 6, 4, 7, 1], Mid, Headers),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  State = maps:get(<<"state">>, BodyDecode),
  [[], [1], [1], [1], [2], [2], [2]] = maps:get(<<"board">>, State),
  1 = maps:get(<<"next_chip">>, State),
  <<"won_by_player1">> = maps:get(<<"status">>, BodyDecode),
  ok.

%% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_right_diagonally(config()) -> ok.
wins_right_diagonally(Config) ->
  Mid = proplists:get_value(mid, Config),
  Headers = proplists:get_value(headers, Config),
  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([4, 4, 4, 4, 6, 3, 3, 3, 2, 2, 7, 1], Mid, Headers),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  State = maps:get(<<"state">>, BodyDecode),
  [[], [2, 1], [2, 1, 2], [2, 1, 2, 1], [], [1], [1]] =
    maps:get(<<"board">>, State),
  2 = maps:get(<<"next_chip">>, State),
  <<"won_by_player2">> = maps:get(<<"status">>, BodyDecode),
  ok.

%% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_left_diagonally(config()) -> ok.
wins_left_diagonally(Config) ->
  Mid = proplists:get_value(mid, Config),
  Headers = proplists:get_value(headers, Config),
  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([4, 4, 4, 4, 2, 5, 5, 5, 6, 6, 1, 7], Mid, Headers),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  State = maps:get(<<"state">>, BodyDecode),
  [[1], [1], [], [2, 1, 2, 1], [2, 1, 2], [2, 1], []] =
    maps:get(<<"board">>, State),
  2 = maps:get(<<"next_chip">>, State),
  <<"won_by_player2">> = maps:get(<<"status">>, BodyDecode),
  ok.

%% @private
drop_chips([], Mid, Headers) -> 
  api_call(get, "/matches/"++Mid, Headers, #{});
drop_chips([Col|Rest], Mid, Headers) ->
  MoveBody = jiffy:encode(#{column => Col}),
  api_call(put, "/matches/" ++ Mid, Headers, MoveBody),
  drop_chips(Rest, Mid, Headers).

api_call(Method, Url, Headers) ->
  api_call(Method, Url, Headers, "").

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