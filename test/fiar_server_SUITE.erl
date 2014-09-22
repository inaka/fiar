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
%%    <li>Authentication</li>
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
-export([ all/0
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

-export([ start/1
        , get_matches/1
        , get_status/1
        , first_play/1
        , play_bad_id/1
        , wins_vertically/1
        , wins_horizontally/1
        , wins_right_diagonally/1
        , wins_left_diagonally/1
        , start_without_player2/1
        , create_user/1
        , unique_user/1
        ]).

-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

init_per_testcase(create_user, Config) -> basic(Config);
init_per_testcase(unique_user, Config) -> basic(Config);
init_per_testcase(get_matches, Config) -> basic(Config);
init_per_testcase(get_status, Config) -> basic(Config);
init_per_testcase(first_play, Config) -> basic(Config);
init_per_testcase(play_bad_id, Config) -> authenticated(Config);
init_per_testcase(start_without_player2, Config) -> authenticated(Config);
init_per_testcase(wins_vertically, Config) -> authenticated(Config);
init_per_testcase(wins_horizontally, Config) -> authenticated(Config);
init_per_testcase(wins_right_diagonally, Config) -> authenticated(Config);
init_per_testcase(wins_left_diagonally, Config) -> authenticated(Config);
init_per_testcase(start, Config) -> authenticated(Config).

basic(Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Config.

authenticated(Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  %create user 1
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name1 = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name1}),
  {ok, #{status_code := 200, body := User1}} =
    api_call(post, "/users", Headers, UserBody),

  %create user 2
  Name2 = ktn_random:generate(),
  User2Body = jiffy:encode(#{username => Name2}),
  {ok, #{status_code := 200, body := User2}} =
    api_call(post, "/users", Headers, User2Body),
  BodyDecode2 = jiffy:decode(User2, [return_maps]),
  Pass2 = binary_to_list(maps:get(<<"pass">>, BodyDecode2)),

  %create match
  BodyDecode = jiffy:decode(User1, [return_maps]),
  Username = binary_to_list(maps:get(<<"username">>, BodyDecode)),

  Pass = binary_to_list(maps:get(<<"pass">>, BodyDecode)),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  Body = jiffy:encode(#{player2 => Name2}),
  {ok, #{status_code := 200, body := Match}} =
    api_call(post, "/matches", Headers1, Body),
  MatchDecode = jiffy:decode(Match, [return_maps]),
  Mid = integer_to_list(maps:get(<<"id">>, MatchDecode)),

  [{username, Username},
   {pass, Pass},
   {username2, Name2},
   {pass2, Pass2},
   {match_id, Mid} | Config].

end_per_testcase(_, Config) ->
  fiar:stop(),
  Config.

-spec create_user(config()) -> ok.
create_user(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name = ktn_random:generate(),
  Body = jiffy:encode(#{username => Name}),
  {ok, #{status_code := 200}} =
    api_call(post, "/users", Headers, Body),
  ok.

-spec unique_user(config()) -> ok.
unique_user(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name = ktn_random:generate(),
  Body = jiffy:encode(#{username => Name}),
  {ok, #{status_code := 200}} =
    api_call(post, "/users", Headers, Body),
  {ok, #{status_code := 409}} =
    api_call(post, "/users", Headers, Body),
  ok.

-spec start(config()) -> ok.
start(Config) ->
  Username = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass = proplists:get_value(pass, Config),
  Headers = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  Body0 = jiffy:encode(#{player2 => "Coco"}),
  Body1 = jiffy:encode(#{player2 => Player2}),
  {ok, #{status_code := 400}} =
    api_call(post, "/matches", Headers, Body0),
  {ok, #{status_code := 200}} =
    api_call(post, "/matches", Headers, Body1),
  ok.

-spec get_matches(config()) -> true.
get_matches(_Config) ->
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name1 = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name1}),
  {ok, #{status_code := 200, body := User1}} =
    api_call(post, "/users", Headers, UserBody),
  
  Name2 = ktn_random:generate(),
  User2Body = jiffy:encode(#{username => Name2}),
  {ok, #{status_code := 200, body := User2}} =
    api_call(post, "/users", Headers, User2Body),
  
  BodyDecode = jiffy:decode(User1, [return_maps]),
  Username = binary_to_list(maps:get(<<"username">>, BodyDecode)),
  Pass = binary_to_list(maps:get(<<"pass">>, BodyDecode)),

  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  {ok, #{status_code := 200, body := RespBody}} =
    api_call(get, "/matches", Headers1),
  [] = jiffy:decode(RespBody),

  Body0 = jiffy:encode(#{player2 => Name2}),
  {ok, #{status_code := 200}} =
    api_call(post, "/matches", Headers1, Body0),
  {ok, #{status_code := 200, body := RespBody1}} =
    api_call(get, "/matches", Headers1),
  [_] = jiffy:decode(RespBody1),

  BodyDecode2 = jiffy:decode(User2, [return_maps]),
  Username2 = binary_to_list(maps:get(<<"username">>, BodyDecode2)),
  Pass2 = binary_to_list(maps:get(<<"pass">>, BodyDecode2)),

  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username2, Pass2}},

  {ok, #{status_code := 200, body := RespBody2}} =
    api_call(get, "/matches", Headers2),
  [_] = jiffy:decode(RespBody2).

-spec get_status(config()) -> ok.
get_status(_Config) ->
  %create user
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name1 = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name1}),
  {ok, #{status_code := 200, body := User}} =
    api_call(post, "/users", Headers, UserBody),

  %create match
  BodyDecode = jiffy:decode(User, [return_maps]),
  Username = binary_to_list(maps:get(<<"username">>, BodyDecode)),
  Pass = binary_to_list(maps:get(<<"pass">>, BodyDecode)),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  Body = jiffy:encode(#{player2 => Name1}),
  {ok, #{status_code := 200, body := MatchBody}} =
    api_call(post, "/matches", Headers1, Body),

  %get status
  Mid =
    integer_to_list(maps:get(<<"id">>, jiffy:decode(MatchBody, [return_maps]))),
  {ok, #{status_code := 200, body := RespBody}} =
    api_call(get, "/matches/"++Mid, Headers1, #{}),
  <<"on_course">> =
    maps:get(<<"status">>, jiffy:decode(RespBody, [return_maps])),
  ok.

-spec first_play(config()) -> ok.
first_play(_Config) ->
  %create user
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name1 = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name1}),
  {ok, #{status_code := 200, body := User}} =
    api_call(post, "/users", Headers, UserBody),

  %create match
  BodyDecode = jiffy:decode(User, [return_maps]),
  Username = binary_to_list(maps:get(<<"username">>, BodyDecode)),
  Pass = binary_to_list(maps:get(<<"pass">>, BodyDecode)),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  Body = jiffy:encode(#{player2 => Name1}),
  {ok, #{status_code := 200, body := MatchBody}} =
    api_call(post, "/matches", Headers1, Body),

  %play
  MatchBodyDecode = jiffy:decode(MatchBody, [return_maps]),
  Mid = integer_to_list(maps:get(<<"id">>, MatchBodyDecode)),
  MoveBody = jiffy:encode(#{column => 1}),
  {ok, #{status_code := 200, body := RespBody}} = 
    api_call(put, "/matches/" ++ Mid, Headers1, MoveBody),
  State = maps:get(<<"state">>, jiffy:decode(RespBody, [return_maps])),
  [[1]|_] = maps:get(<<"board">>, State),
  2 = maps:get(<<"next_chip">>, State),
  ok.

-spec play_bad_id(config()) -> ok.
play_bad_id(Config) ->
  Player1 = proplists:get_value(username, Config),
  Pass = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass}},
  MoveBody = jiffy:encode(#{column => 1}),
  {ok, #{status_code := 400}} = 
    api_call(put, "/matches/id", Headers1, MoveBody),
  {ok, #{status_code := 404}} = 
    api_call(put, "/matches/123456789", Headers1, MoveBody),
  ok.

-spec start_without_player2(config()) -> ok.
start_without_player2(Config) ->
  Player1 = proplists:get_value(username, Config),
  Pass = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass}},
  {ok, #{status_code := 400}} =
    api_call(post, "/matches", Headers1, jiffy:encode(#{})),
  ok.

%% @doc when the player puts 4 chips in a vertical row, wins
-spec wins_vertically(config()) -> ok.
wins_vertically(Config) ->
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},              
  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([1, 2, 1, 2, 1, 2, 1], Mid, [Headers1, Headers2]),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  State = maps:get(<<"state">>, BodyDecode),
  [[1, 1, 1], [2, 2, 2], [], [], [], [], []] = maps:get(<<"board">>, State),
  1 = maps:get(<<"next_chip">>, State),
  <<"won_by_player1">> = maps:get(<<"status">>, BodyDecode),
  ok.

% @doc when the player puts 4 chips in a horizontal row, wins
-spec wins_horizontally(config()) -> ok.
wins_horizontally(Config) ->
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}}, 

  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([2, 5, 3, 6, 4, 7, 1], Mid, [Headers1, Headers2]),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  State = maps:get(<<"state">>, BodyDecode),
  [[], [1], [1], [1], [2], [2], [2]] = maps:get(<<"board">>, State),
  1 = maps:get(<<"next_chip">>, State),
  <<"won_by_player1">> = maps:get(<<"status">>, BodyDecode),
  ok.

%% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_right_diagonally(config()) -> ok.
wins_right_diagonally(Config) ->
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},
  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([4, 4, 4, 4, 6, 3, 3, 3, 2, 2, 7, 1], Mid, [Headers1, Headers2]),
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
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},
  {ok, #{status_code := 200, body := RespBody}} =
    drop_chips([4, 4, 4, 4, 2, 5, 5, 5, 6, 6, 1, 7], Mid, [Headers1, Headers2]),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  State = maps:get(<<"state">>, BodyDecode),
  [[1], [1], [], [2, 1, 2, 1], [2, 1, 2], [2, 1], []] =
    maps:get(<<"board">>, State),
  2 = maps:get(<<"next_chip">>, State),
  <<"won_by_player2">> = maps:get(<<"status">>, BodyDecode),
  ok.

%% @private
drop_chips([], Mid, [Header, _]) -> 
  api_call(get, "/matches/"++Mid, Header, #{});
drop_chips([Col|Rest], Mid, [A, B]) ->
  MoveBody = jiffy:encode(#{column => Col}),
  api_call(put, "/matches/" ++ Mid, A, MoveBody),
  drop_chips(Rest, Mid, [B, A]).

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