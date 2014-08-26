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
%%    </ul>
%%
%%    == Problem Statement ==
%%    Create a 4-in-a-row game manager
%%    <dl>
%%    <dt>start() -> board().</dt><dd>Returns an empty board</dd>
%%    <dt>play(Col, Board) -> won | drawn | {next, board()}.</dt>
%%      <dd>Accepts a play and returns:
%%        <dl>
%%          <dt>won</dt><dd>the player won</dd>
%%          <dt>drawn</dt><dd>the game ended in a draw</dd>
%%          <dt>next</dt><dd>the game should keep going</dd>
%%        </dl>
%%      </dd>
%%    </dl>
-module(fiar_match_SUITE).
-author('elbrujohalcon@inaka.net').

-type config() :: [{atom(), term()}].
-export([all/0, init_per_testcase/2, end_per_testcase/2]).
-export([empty/1, invalid/1, drawn/1, stop/1]).
-export([wins_vertically/1, wins_horizontally/1, 
         wins_right_diagonally/1, wins_left_diagonally/1]).

init_per_testcase(_, Config) ->
  {ok, Pid} = fiar_match:start(),
  [{pid, Pid} | Config].

end_per_testcase(_, Config) ->
  Pid = proplists:get_value(pid, Config),
  fiar_match:stop(Pid),
  Config.

%% @private
-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

%% @doc start returns an empty board
-spec empty(config()) -> ok.
empty(Config) ->
  Pid = proplists:get_value(pid, Config),
  lists:foreach(
    fun(Col) ->
      next = fiar_match:play(Pid, Col)
    end,
    [1, 2, 3, 4, 5, 6, 7]),
  ok.

%% @doc no row accepts more than 7 chips
-spec invalid(config()) -> ok.
invalid(Config) ->
  Pid = proplists:get_value(pid, Config),
  drop_chips(7, 1, Pid),
  try fiar_match:play(Pid, 1) of
    Result -> no_result = Result
  catch
    invalid_column -> invalid_column
  end,
  ok.

%% @doc when the board is full, play should always return drawn
 -spec drawn(config()) -> ok.
 drawn(Config) ->
   Pid = proplists:get_value(pid, Config),
   almost_fill_board(Pid),
   drawn = fiar_match:play(Pid, 3),
   ok.

% %% @doc when the player puts 4 chips in a vertical row, wins
-spec wins_vertically(config()) -> ok.
wins_vertically(Config) ->
  Pid = proplists:get_value(pid, Config),
  drop_chips([1, 2, 1, 2, 1, 2], Pid),
  won = fiar_match:play(Pid, 1),
  ok.

% %% @doc when the player puts 4 chips in a horizontal row, wins
-spec wins_horizontally(config()) -> ok.
wins_horizontally(Config) ->
  Pid = proplists:get_value(pid, Config),
  drop_chips([2, 5, 3, 6, 4, 7], Pid),
  won = fiar_match:play(Pid, 1),
  ok.

% %% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_right_diagonally(config()) -> ok.
wins_right_diagonally(Config) ->
  Pid = proplists:get_value(pid, Config),
  drop_chips([4, 4, 4, 4, 6, 3, 3, 3, 2, 2, 7], Pid),
  won = fiar_match:play(Pid, 1),
  ok.

%% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_left_diagonally(config()) -> ok.
wins_left_diagonally(Config) ->
 Pid = proplists:get_value(pid, Config),
 drop_chips([4, 4, 4, 4, 2, 5, 5, 5, 6, 6, 1], Pid),
 won = fiar_match:play(Pid, 7),
 ok.

%% @doc test the server stoped
-spec stop(config()) -> ok.
stop(Config) ->
  Pid = proplists:get_value(pid, Config),
  fiar_match:stop(Pid),
  try fiar_match:play(Pid, 1) of
    Result -> no_result = Result
  catch
    _:Ex -> ok
  end.

%% @private
%% @doc fills all columns in the board except #3
almost_fill_board(Pid) ->
 lists:foldl(fun fill_column/2, Pid, [1, 4, 2, 5, 6, 7]),
 drop_chips(6, 3, Pid).

%% @private
fill_column(Col, Pid) -> drop_chips(7, Col, Pid).

%% @private
drop_chips(0, _Col, Pid) -> Pid;
drop_chips(N, Col, Pid) ->
  {Col, Pid, next} = {Col, Pid, fiar_match:play(Pid, Col)},
  drop_chips(N-1, Col, Pid).

%% @private
drop_chips([], Pid) -> Pid;
drop_chips([Col|Rest], Pid) ->
 {Col, next} = {Col, fiar_match:play(Pid, Col)},
 drop_chips(Rest, Pid).

