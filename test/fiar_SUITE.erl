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
-export([start/1, stop/1]).
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([empty/1, invalid/1, drawn/1]).
-export([wins_vertically/1, wins_horizontally/1, 
         wins_right_diagonally/1, wins_left_diagonally/1]).
-export([two_matches/1, match_status/1]).


%% @private
-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

init_per_testcase(start, Config) ->
  Config;
init_per_testcase(two_matches, Config) ->
  ok = fiar:start(),
  Match1 = fiar:start_match("Brujo", "Euen"),
  Match2 = fiar:start_match("Fede", "Juan"),
  [{match1, Match1}, {match2, Match2} | Config];
init_per_testcase(_, Config) ->
  ok = fiar:start(),
  Match1 = fiar:start_match("Brujo", "Euen"),
  [{match1, Match1} | Config].

end_per_testcase(stop, Config) ->
  Config;
end_per_testcase(_, Config) ->
  fiar:stop(),
  Config.

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

%% @doc no row accepts more than 7 chips
-spec invalid(config()) -> ok.
invalid(Config) ->
  Match1 = proplists:get_value(match1, Config),
  drop_chips(7, 1, Match1),
  try fiar:play(Match1, 1) of
    Result -> no_result = Result
  catch
    invalid_column -> invalid_column
  end,
  ok.

%% @doc when the board is full, play should always return drawn
 -spec drawn(config()) -> ok.
 drawn(Config) ->
   Match1 = proplists:get_value(match1, Config),
   almost_fill_board(Match1),
   drawn = fiar:play(Match1, 3),
   drawn = fiar:match_status(Match1),
   ok.

% %% @doc when the player puts 4 chips in a vertical row, wins
-spec wins_vertically(config()) -> ok.
wins_vertically(Config) ->
  Match1 = proplists:get_value(match1, Config),
  drop_chips([1, 2, 1, 2, 1, 2], Match1),
  won = fiar:play(Match1, 1),
  {won_by, "Brujo"} = fiar:match_status(Match1),
  ok.

% %% @doc when the player puts 4 chips in a horizontal row, wins
-spec wins_horizontally(config()) -> ok.
wins_horizontally(Config) ->
  Match1 = proplists:get_value(match1, Config),
  drop_chips([2, 5, 3, 6, 4, 7], Match1),
  won = fiar:play(Match1, 1),
  {won_by, _} = fiar:match_status(Match1),
  ok.

% %% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_right_diagonally(config()) -> ok.
wins_right_diagonally(Config) ->
  Match1 = proplists:get_value(match1, Config),
  drop_chips([4, 4, 4, 4, 6, 3, 3, 3, 2, 2, 7], Match1),
  won = fiar:play(Match1, 1),
  {won_by, _} = fiar:match_status(Match1),
  ok.

%% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_left_diagonally(config()) -> ok.
wins_left_diagonally(Config) ->
 Match1 = proplists:get_value(match1, Config),
 drop_chips([4, 4, 4, 4, 2, 5, 5, 5, 6, 6, 1], Match1),
 won = fiar:play(Match1, 7),
 {won_by, _} = fiar:match_status(Match1),
 ok.


%% @doc test run two matches simultaneously
-spec two_matches(config()) -> ok.
two_matches(Config) ->
  Match1 = proplists:get_value(match1, Config),
  Match2 = proplists:get_value(match2, Config),
  fill_column(1, Match1),
  drop_chips([1, 2, 1, 2, 1, 2], Match2),
  won = fiar:play(Match2, 1),
  {won_by, _} = fiar:match_status(Match2),
  try fiar:play(Match1, 1) of
    Result -> no_result = Result
  catch
    _:_ -> ok
  end.
  
match_status(Config) ->
  Match1 = proplists:get_value(match1, Config),
  {next_player, "Brujo"} = fiar:match_status(Match1),
  ok = fiar:stop(),
  ok = fiar:start(),
  {next_player, "Brujo"} = fiar:match_status(Match1),
  next = fiar:play(Match1, 5),
  {next_player, "Euen"} = fiar:match_status(Match1),
  ok = fiar:stop(),
  ok = fiar:start(),
  {next_player, "Euen"} = fiar:match_status(Match1),
  ok.

%% @private
%% @doc fills all columns in the board except #3
almost_fill_board(Match) ->
  lists:foldl(fun fill_column/2, Match, [1, 4, 2, 5, 6, 7]),
  drop_chips(6, 3, Match).

%% @private
fill_column(Col, Match) -> drop_chips(7, Col, Match).

%% @private
drop_chips(0, _Col, Match) -> Match;
drop_chips(N, Col, Match) ->
  {Col, Match, next} = {Col, Match, fiar:play(Match, Col)},
  drop_chips(N-1, Col, Match).

%% @private
drop_chips([], Match) -> Match;
drop_chips([Col|Rest], Match) ->
  {Col, next} = {Col, fiar:play(Match, Col)},
  drop_chips(Rest, Match).

%% @private
fiar_app() ->
  [fiar || {fiar, _, _} <- application:which_applications()].