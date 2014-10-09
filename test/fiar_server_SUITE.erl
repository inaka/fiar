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
        , bad_credentials/1
        , complete_coverage/1
        , invalid/1
        , drawn/1
        , forbidden_move/1
        , match_finished/1
        , notification_of_move/1
        , get_event_invalid_player/1
        , double_connection/1
        , get_events/1
        , get_connections/1
        , user_disconnected/1
        , match_created_event/1
        , match_ended_event/1
        ]).

-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

init_per_testcase(create_user, Config) -> basic(Config);
init_per_testcase(unique_user, Config) -> basic(Config);
init_per_testcase(get_matches, Config) -> basic(Config);
init_per_testcase(get_status, Config) -> basic(Config);
init_per_testcase(first_play, Config) -> basic(Config);
init_per_testcase(bad_credentials, Config) -> basic(Config);
init_per_testcase(complete_coverage, Config) -> authenticated(Config);
init_per_testcase(play_bad_id, Config) -> authenticated(Config);
init_per_testcase(start_without_player2, Config) -> authenticated(Config);
init_per_testcase(wins_vertically, Config) -> authenticated(Config);
init_per_testcase(wins_horizontally, Config) -> authenticated(Config);
init_per_testcase(wins_right_diagonally, Config) -> authenticated(Config);
init_per_testcase(wins_left_diagonally, Config) -> authenticated(Config);
init_per_testcase(invalid, Config) -> authenticated(Config);
init_per_testcase(forbidden_move, Config) -> authenticated(Config);
init_per_testcase(match_finished, Config) -> authenticated(Config);
init_per_testcase(drawn, Config) -> authenticated(Config);
init_per_testcase(notification_of_move, Config) -> authenticated(Config);
init_per_testcase(get_event_invalid_player, Config) -> authenticated(Config);
init_per_testcase(double_connection, Config) -> authenticated(Config);
init_per_testcase(get_events, Config) -> authenticated(Config);
init_per_testcase(get_connections, Config) -> authenticated(Config);
init_per_testcase(user_disconnected, Config) -> authenticated(Config);
init_per_testcase(match_created_event, Config) -> authenticated(Config);
init_per_testcase(match_ended_event, Config) -> authenticated(Config);
init_per_testcase(start, Config) -> authenticated(Config).

basic(Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  Config.

authenticated(Config) ->
  {ok, _} = application:ensure_all_started(shotgun),
  ok = fiar:start(),
  % Create user 1
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name1 = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name1}),
  {ok, #{status_code := 200, body := User1}} =
    api_call(post, "/users", Headers, UserBody),

  % Ccreate user 2
  Name2 = ktn_random:generate(),
  User2Body = jiffy:encode(#{username => Name2}),
  {ok, #{status_code := 200, body := User2}} =
    api_call(post, "/users", Headers, User2Body),
  BodyDecode2 = jiffy:decode(User2, [return_maps]),
  Pass2 = maps:get(<<"pass">>, BodyDecode2),

  % Create match
  BodyDecode = jiffy:decode(User1, [return_maps]),
  Username = maps:get(<<"username">>, BodyDecode),
  Id1 = maps:get(<<"id">>, BodyDecode),

  Pass = maps:get(<<"pass">>, BodyDecode),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  Body = jiffy:encode(#{player2 => Name2}),
  {ok, #{status_code := 200, body := Match}} =
    api_call(post, "/matches", Headers1, Body),
  MatchDecode = jiffy:decode(Match, [return_maps]),
  Mid = integer_to_list(maps:get(<<"id">>, MatchDecode)),

  [{username, Name2},
   {pass, Pass2},
   {id1, Id1},
   {username2, Username},
   {pass2, Pass},
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

-spec bad_credentials(config()) -> ok.
bad_credentials(_Config) ->
  % Without auth
  Headers = #{<<"content-type">> => <<"application/json">>},  
  {ok, #{status_code := 401}} =
         api_call(get, "/matches", Headers),
  % Bad user and pass
  HeadersInv = #{<<"content-type">> => <<"application/json">>,
               basic_auth => {"user", "pass"}},
  {ok, #{status_code := 401}} =
         api_call(get, "/matches", HeadersInv),
  % Crate user
  Name = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name}),
  {ok, #{status_code := 200, body := User}} =
         api_call(post, "/users", Headers, UserBody),
  BodyDecode = jiffy:decode(User, [return_maps]),         
  Pass = maps:get(<<"pass">>, BodyDecode),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
               basic_auth => {Name, Pass}},
  % Get match with bad id
  {ok, #{status_code := 404}} =
         api_call(get, "/matches/123456", Headers1),
  {ok, #{status_code := 404}} =
         api_call(get, "/matches/id", Headers1),
  {ok, #{status_code := 401}} =
         api_call(get, "/matches/123456", Headers),
  {ok, #{status_code := 401}} =
         api_call(get, "/matches/id", Headers),
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
  Username = maps:get(<<"username">>, BodyDecode),
  Pass = maps:get(<<"pass">>, BodyDecode),

  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  {ok, #{status_code := 200, body := RespBody}} =
    api_call(get, "/matches", Headers1),
  [] = jiffy:decode(RespBody),

  Body0 = jiffy:encode(#{player2 => Name2}),
  {ok, #{status_code := 200, body := MatchBody0}} =
    api_call(post, "/matches", Headers1, Body0),
  {ok, #{status_code := 200, body := RespBody1}} =
    api_call(get, "/matches", Headers1),
  #{<<"id">> := Mid} = jiffy:decode(MatchBody0, [return_maps]),
  [_] = jiffy:decode(RespBody1),

  BodyDecode2 = jiffy:decode(User2, [return_maps]),
  Username2 = maps:get(<<"username">>, BodyDecode2),
  Pass2 = maps:get(<<"pass">>, BodyDecode2),

  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username2, Pass2}},

  {ok, #{status_code := 200, body := RespBody2}} =
    api_call(get, "/matches", Headers2),
  [_] = jiffy:decode(RespBody2),

  Name3 = ktn_random:generate(),
  User3Body = jiffy:encode(#{username => Name3}),
  {ok, #{status_code := 200, body := User3}} =
    api_call(post, "/users", Headers, User3Body),

  BodyDecode3 = jiffy:decode(User3, [return_maps]),
  Pass3 = maps:get(<<"pass">>, BodyDecode3),

  Headers3 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Name3, Pass3}},

  {ok, #{status_code := 200, body := RespBody3}} =
    api_call(get, "/matches", Headers3),
  [] = jiffy:decode(RespBody3),
    
  {ok, #{status_code := 200}} =
    api_call(get, "/matches/" ++ integer_to_list(Mid), Headers1),

  {ok, #{status_code := 200}} =
    api_call(get, "/matches/" ++ integer_to_list(Mid), Headers2),

  {ok, #{status_code := 404}} =
    api_call(get, "/matches/" ++ integer_to_list(Mid), Headers3),
  ok.

-spec get_status(config()) -> ok.
get_status(_Config) ->
  % Create user
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name1 = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name1}),
  {ok, #{status_code := 200, body := User}} =
    api_call(post, "/users", Headers, UserBody),
  % Create match
  BodyDecode = jiffy:decode(User, [return_maps]),
  Username = maps:get(<<"username">>, BodyDecode),
  Pass = maps:get(<<"pass">>, BodyDecode),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  Body = jiffy:encode(#{player2 => Name1}),
  {ok, #{status_code := 200, body := MatchBody}} =
    api_call(post, "/matches", Headers1, Body),
  % Get status
  Mid =
    integer_to_list(maps:get(<<"id">>, jiffy:decode(MatchBody, [return_maps]))),
  {ok, #{status_code := 200, body := RespBody}} =
    api_call(get, "/matches/"++Mid, Headers1, #{}),
  <<"on_course">> =
    maps:get(<<"status">>, jiffy:decode(RespBody, [return_maps])),
  ok.

-spec first_play(config()) -> ok.
first_play(_Config) ->
  % Create user
  Headers = #{<<"content-type">> => <<"application/json">>},
  Name1 = ktn_random:generate(),
  UserBody = jiffy:encode(#{username => Name1}),
  {ok, #{status_code := 200, body := User}} =
    api_call(post, "/users", Headers, UserBody),
  % Create match
  BodyDecode = jiffy:decode(User, [return_maps]),
  Username = maps:get(<<"username">>, BodyDecode),
  Pass = maps:get(<<"pass">>, BodyDecode),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Username, Pass}},
  Body = jiffy:encode(#{player2 => Name1}),
  {ok, #{status_code := 200, body := MatchBody}} =
    api_call(post, "/matches", Headers1, Body),
  % Play
  MatchBodyDecode = jiffy:decode(MatchBody, [return_maps]),
  Mid = integer_to_list(maps:get(<<"id">>, MatchBodyDecode)),
  MoveBody = jiffy:encode(#{column => 1}),
  {ok, #{status_code := 200, body := RespBody}} = 
    api_call(put, "/matches/" ++ Mid, Headers1, MoveBody),
  State = maps:get(<<"state">>, jiffy:decode(RespBody, [return_maps])),
  [[2]|_] = maps:get(<<"board">>, State),
  1 = maps:get(<<"next_chip">>, State),
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
  [[2, 2, 2], [1, 1, 1], [], [], [], [], []] = maps:get(<<"board">>, State),
  2 = maps:get(<<"next_chip">>, State),
  <<"won_by_player2">> = maps:get(<<"status">>, BodyDecode),
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
  [[], [2], [2], [2], [1], [1], [1]] = maps:get(<<"board">>, State),
  2 = maps:get(<<"next_chip">>, State),
  <<"won_by_player2">> = maps:get(<<"status">>, BodyDecode),
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
  [[], [1, 2], [1, 2, 1], [1, 2, 1, 2], [], [2], [2]] =
    maps:get(<<"board">>, State),
  1 = maps:get(<<"next_chip">>, State),
  <<"won_by_player1">> = maps:get(<<"status">>, BodyDecode),
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
  [[2], [2], [], [1, 2, 1, 2], [1, 2, 1], [1, 2], []] =
    maps:get(<<"board">>, State),
  1 = maps:get(<<"next_chip">>, State),
  <<"won_by_player1">> = maps:get(<<"status">>, BodyDecode),
  ok.

-spec match_finished(config()) -> ok.
match_finished(Config) ->
  %% First, let's finish a match...
  ok = wins_vertically(Config),

  %% Now, let's try to keep playing...
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},
  MoveBody = jiffy:encode(#{column => 1}),

  {ok, #{status_code := 400}} =
    api_call(put, "/matches/" ++ Mid, Headers1, MoveBody),
  {ok, #{status_code := 400}} =
    api_call(put, "/matches/" ++ Mid, Headers2, MoveBody),
  ok.

-spec forbidden_move(config()) -> ok.
forbidden_move(Config) ->
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Pass1 = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  MoveBody = jiffy:encode(#{column => 1}),
  {ok, #{status_code := 200}} =
    api_call(put, "/matches/" ++ Mid, Headers1, MoveBody),
  {ok, #{status_code := 403}} =
    api_call(put, "/matches/" ++ Mid, Headers1, MoveBody),
  ok.

-spec invalid(config()) -> ok.
invalid(Config) ->
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},
  {ok, #{status_code := 200}} =
    drop_chips([1, 1, 1, 1, 1, 1, 1], Mid, [Headers1, Headers2]),
  MoveBody = jiffy:encode(#{column => 1}),
  {ok, #{status_code := 400}} =
    api_call(put, "/matches/" ++ Mid, Headers2, MoveBody),
  MoveBody1 = jiffy:encode(#{column => 9}),
  {ok, #{status_code := 400}} =
    api_call(put, "/matches/" ++ Mid, Headers2, MoveBody1),
  ok.

-spec drawn(config()) -> ok.
drawn(Config) ->
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
    drop_chips([1, 1, 1, 1, 1, 1, 1, 
                4, 4, 4, 4, 4, 4, 4,
                2, 2, 2, 2, 2, 2, 2,
                5, 5, 5, 5, 5, 5, 5,
                6, 6, 6, 6, 6, 6, 6,
                7, 7, 7, 7, 7, 7, 7,
                3, 3, 3, 3, 3, 3, 3],
                Mid,
                [Headers1, Headers2]),
  BodyDecode = jiffy:decode(RespBody, [return_maps]),
  <<"drawn">> = maps:get(<<"status">>, BodyDecode),
  ok.

-spec notification_of_move(config()) -> ok.
notification_of_move(Config) ->
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
               basic_auth => {Player2, Pass2}},
  Body = jiffy:encode(#{column => 1}),
  {ok, Pid1} = shotgun:open("localhost", 8080),
  {ok, Pid2} = shotgun:open("localhost", 8080),
  try
    {ok, _Ref} = shotgun:get( Pid1
                            , "/matches/" ++ Mid ++ "/events"
                            , Headers2
                            , #{ async => true
                               , async_mode => sse}),
    api_call(put, "/matches/" ++ Mid, Headers, Body),
    timer:sleep(300),
    {ok, _Ref1} = shotgun:get( Pid2
                             , "/matches/" ++ Mid ++ "/events"
                             , Headers
                             , #{ async => true
                                , async_mode => sse}),
    api_call(put, "/matches/" ++ Mid, Headers2, Body),
    timer:sleep(300),
    [{_, _, EventBin2}] = shotgun:events(Pid2),
    <<"turn">> = maps:get(event, shotgun:parse_event(EventBin2)),
    api_call(put, "/matches/" ++ Mid, Headers, Body),
    timer:sleep(300),
    [{ _, _, EventBin3 }, { _, _, EventBin4 }] = shotgun:events(Pid1),
    <<"turn">> = maps:get(event, shotgun:parse_event(EventBin3)),
    <<"turn">> = maps:get(event, shotgun:parse_event(EventBin4))
  catch
    _:Ex -> throw({error, Ex})
  after
    shotgun:close(Pid1),
    shotgun:close(Pid2)
  end.

-spec double_connection(config()) -> ok.
double_connection(Config) ->
  % Get match and player1
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Pass1 = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Player2 = proplists:get_value(username2, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},
  Body = jiffy:encode(#{column => 1}),

  {ok, Pid1} = shotgun:open("localhost", 8080),
  {ok, Pid2} = shotgun:open("localhost", 8080),
  % Get event with the same player and match
  try
    {ok, Ref1} = shotgun:get( Pid1
                            , "/matches/" ++ Mid ++ "/events"
                            , Headers2
                            , #{ async => true
                               , async_mode => sse}),

    api_call(put, "/matches/" ++ Mid, Headers1, Body),

    timer:sleep(300),
    [{nofin, Ref1, _}] = shotgun:events(Pid1),

    {ok, Ref2} = shotgun:get( Pid2
                            , "/matches/" ++ Mid ++ "/events"
                            , Headers2
                            , #{ async => true
                               , async_mode => sse}),

    api_call(put, "/matches/" ++ Mid, Headers2, Body),
    api_call(put, "/matches/" ++ Mid, Headers1, Body),

    timer:sleep(300),
    [] = shotgun:events(Pid1),
    [{nofin, Ref2, _}] = shotgun:events(Pid2),
    ok
  catch
    _:Ex -> throw({error, Ex})
  after
    shotgun:close(Pid1),
    shotgun:close(Pid2)
  end.

-spec get_event_invalid_player(config()) -> ok.
get_event_invalid_player(Config) ->
  % Get match and player1
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Pass1 = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  % Create player2
  Headers = #{<<"content-type">> => <<"application/json">>},
  Player2 = ktn_random:generate(),
  User2Body = jiffy:encode(#{username => Player2}),
  {ok, #{status_code := 200, body := User2}} =
    api_call(post, "/users", Headers, User2Body),
  BodyDecode2 = jiffy:decode(User2, [return_maps]),
  Pass2 = maps:get(<<"pass">>, BodyDecode2),
  Headers2 = #{<<"content-type">> => <<"application/json">>,
               basic_auth => {Player2, Pass2}},
  % Get event with invalid player
  {ok, Pid1} = shotgun:open("localhost", 8080),
  {ok, Pid2} = shotgun:open("localhost", 8080),
  {ok, Pid3} = shotgun:open("localhost", 8080),
  try
    shotgun:get( Pid1
               , "/matches/" ++ Mid ++ "/events"
               , Headers2
               , #{ async => true
                  , async_mode => sse}),
    timer:sleep(500),
    [Status1] = shotgun:events(Pid1),
    404 = maps:get(status_code, Status1),
    % Get event with invalid id
    shotgun:get( Pid2
               , "/matches/123456/events"
               , Headers1
               , #{ async => true
                  , async_mode => sse}),
    timer:sleep(500),
    [Status2] = shotgun:events(Pid2),
    404 = maps:get(status_code, Status2),
    % Get event with player not authenticated
    shotgun:get( Pid3
               , "/matches/" ++ Mid ++ "/events"
               , Headers
               , #{ async => true
                  , async_mode => sse}),
    timer:sleep(500),
    [Status3] = shotgun:events(Pid3),
    401 = maps:get(status_code, Status3),
    ok
  catch
    _:Ex -> {error, Ex}
  after
    shotgun:close(Pid1),
    shotgun:close(Pid2),
    shotgun:close(Pid3)
  end.

-spec get_events(config()) -> ok.
get_events(Config) ->
  % Get match and player1
  Player1 = proplists:get_value(username, Config),
  Pass1 = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},

  {ok, Pid1} = shotgun:open("localhost", 8080),
  try
    {ok, Ref1} = shotgun:get( Pid1
                            , "/events"
                            , Headers1
                            , #{ async => true
                               , async_mode => sse}),
    timer:sleep(500),
    true = erlang:is_list(shotgun:events(Pid1)),
    ok
  catch
    _:Ex -> throw({error, Ex})
  after
    shotgun:close(Pid1)
  end.

-spec get_connections(config()) -> ok.
get_connections(Config) ->
  % Get match and player1
  Player1 = proplists:get_value(username, Config),
  Pass1 = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  % Get events
  {ok, Pid1} = shotgun:open("localhost", 8080),
  try
    {ok, _Ref1} = shotgun:get( Pid1
                            , "/events"
                            , Headers1
                            , #{ async => true
                               , async_mode => sse}),
    timer:sleep(500),
    [{ _, _, EventBin1}, { _, _, EventBin2}] = shotgun:events(Pid1),
    Event1 = shotgun:parse_event(EventBin1),
    [Data] = jiffy:decode(maps:get(data, Event1), [return_maps]),
    [] = maps:get(<<"current_match">>, Data),
    true = is_map(maps:get(<<"user">>, Data)),
    Event2 = shotgun:parse_event(EventBin2),
    <<"user_conected">> = maps:get(event, Event2),
    ok
  catch
    _:Ex -> throw({error, Ex})
  after
    shotgun:close(Pid1)
  end.


-spec user_disconnected(config()) -> ok.
user_disconnected(Config) ->
  % Get match and player1
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},
  % Get events
  {ok, Pid1} = shotgun:open("localhost", 8080),
  {ok, Pid2} = shotgun:open("localhost", 8080),
  try
    {ok, _} = shotgun:get( Pid1
                            , "/events"
                            , Headers1
                            , #{ async => true
                               , async_mode => sse}),
    timer:sleep(500),
    {ok, _} = shotgun:get( Pid2
                            , "/events"
                            , Headers2
                            , #{ async => true
                               , async_mode => sse}),
    timer:sleep(500),
 
    [{ _, _, EventBin1}, { _, _, EventBin2}, _] = shotgun:events(Pid1),
    Event1 = shotgun:parse_event(EventBin1),
    [Data] = jiffy:decode(maps:get(data, Event1), [return_maps]),
    [] = maps:get(<<"current_match">>, Data),
    true = is_map(maps:get(<<"user">>, Data)),

    Event2 = shotgun:parse_event(EventBin2),
    <<"user_conected">> = maps:get(event, Event2),
    shotgun:close(Pid1),

    timer:sleep(500),
    [_, _, { _, _, EventBin3}] = shotgun:events(Pid2),
    Event3 = shotgun:parse_event(EventBin3),
    <<"user_disconnected">> = maps:get(event, Event3),
    ok
  catch
    _:Ex -> throw({error, Ex})
  after
    shotgun:close(Pid1),
    shotgun:close(Pid2)
  end.

-spec match_created_event(config()) -> ok.
match_created_event(Config) ->
  % Get match and player1
  Player1 = proplists:get_value(username, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass1 = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  % Get events
  {ok, Pid1} = shotgun:open("localhost", 8080),
  try
    {ok, _} = shotgun:get( Pid1
                            , "/events"
                            , Headers1
                            , #{ async => true
                               , async_mode => sse}),
    timer:sleep(500),
    [{ _, _, EventBin1}, { _, _, EventBin2}] = shotgun:events(Pid1),
    Event1 = shotgun:parse_event(EventBin1),
    [_] = jiffy:decode(maps:get(data, Event1), [return_maps]),
    Event2 = shotgun:parse_event(EventBin2),
    <<"user_conected">> = maps:get(event, Event2),
    % % Create match
    Body = jiffy:encode(#{player2 => Player2}),
    {ok, #{status_code := 200, body := Match}} =
      api_call(post, "/matches", Headers1, Body),
    timer:sleep(500),
    [{ _, _, EventBin3}] = shotgun:events(Pid1),
    Event3 = shotgun:parse_event(EventBin3),
    <<"match_started">> = maps:get(event, Event3),
    ok
  catch
    _:Ex -> throw({error, Ex})
  after
    shotgun:close(Pid1)
  end.

-spec match_ended_event(config()) -> ok.
match_ended_event(Config) ->
  % Get match and player1
  Mid = proplists:get_value(match_id, Config),
  Player1 = proplists:get_value(username, Config),
  Pass1 = proplists:get_value(pass, Config),
  Headers1 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player1, Pass1}},
  % Get events
  {ok, Pid1} = shotgun:open("localhost", 8080),
  try
    {ok, _} = shotgun:get( Pid1
                            , "/events"
                            , Headers1
                            , #{ async => true
                               , async_mode => sse}),
    timer:sleep(500),
    [{ _, _, EventBin1}, { _, _, EventBin2}] = shotgun:events(Pid1),
    Event1 = shotgun:parse_event(EventBin1),
    [_] = jiffy:decode(maps:get(data, Event1), [return_maps]),
    Event2 = shotgun:parse_event(EventBin2),
    <<"user_conected">> = maps:get(event, Event2),
    % Terminate match
    ok = wins_vertically(Config),
    timer:sleep(500),
    [{ _, _, EventBin3}] = shotgun:events(Pid1),
    Event3 = shotgun:parse_event(EventBin3),
    <<"match_ended">> = maps:get(event, Event3),
    ok
  catch
    _:Ex -> throw({error, Ex})
  after
    shotgun:close(Pid1)
  end.




-spec complete_coverage(config()) -> ok.
complete_coverage(Config) ->
  try fiar_auth:credentials({bad_attribute}) of
    Credentials -> throw({error, Credentials})
  catch
    _ -> ok
  end,

  Mid = proplists:get_value(match_id, Config),
  Id1 = proplists:get_value(id1, Config),
  Player2 = proplists:get_value(username2, Config),
  Pass2 = proplists:get_value(pass2, Config),
  Headers2 = #{<<"content-type">> => <<"application/json">>,
              basic_auth => {Player2, Pass2}},
  {ok, Pid1} = shotgun:open("localhost", 8080),
  try
    shotgun:get( Pid1
               , "/matches/" ++ Mid ++ "/events"
               , Headers2
               , #{ async => true
                  , async_mode => sse}),

    ProcessName =
      list_to_atom("fiar_player_" ++ Mid ++ "_" ++ integer_to_list(Id1)),
    timer:sleep(500),
    ProcessName ! completing_coverage_of_handle_info
  after
    shotgun:close(Pid1)
  end,

  state = fiar_notify_handler:handle_error(msg, reason, state),
  ok.

%% @private
drop_chips([], Mid, [Header, _]) -> 
  api_call(get, "/matches/"++Mid, Header, #{});
drop_chips([Col|Rest], Mid, [Header1, Header2]) ->
  MoveBody = jiffy:encode(#{column => Col}),
  api_call(put, "/matches/" ++ Mid, Header1, MoveBody),
  drop_chips(Rest, Mid, [Header2, Header1]).

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