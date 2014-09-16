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
-module(fiar_core_SUITE).
-author('elbrujohalcon@inaka.net').

-type config() :: [{atom(), term()}].
-export([all/0]).
-export([empty/1, invalid/1, drawn/1]).
-export([wins_vertically/1, wins_horizontally/1, 
         wins_right_diagonally/1, wins_left_diagonally/1]).
-export ([fail_board/1]).

%% @private
-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

%% @doc test play with fail board
-spec fail_board(config()) -> ok.
fail_board(_Config) ->
  EmptyBoard = fiar_core:start(),
  try fiar_core:play(8, EmptyBoard) of
      Result -> no_result = Result
  catch
      invalid_column -> ok
  end.

%% @doc start returns an empty board
-spec empty(config()) -> ok.
empty(_Config) ->
  Board = fiar_core:start(),
  lists:foreach(
    fun(Col) ->
      {next, _} = fiar_core:play(Col, Board)
    end,
    [1, 2, 3, 4, 5, 6, 7]),
  ok.

%% @doc no row accepts more than 7 chips
-spec invalid(config()) -> ok.
invalid(_Config) ->
  EmptyBoard = fiar_core:start(),
  BoardAfter7 = drop_chips(7, 1, EmptyBoard),
  try fiar_core:play(1, BoardAfter7) of
    Result ->
      no_result = Result
  catch
    _:Ex ->
      invalid_column = Ex
  end,
  ok.

%% @doc when the board is full, play should always return drawn
-spec drawn(_Config) -> ok.
drawn(_Config) ->
  EmptyBoard = fiar_core:start(),
  FullBoard = almost_fill_board(EmptyBoard),
  drawn = fiar_core:play(3, FullBoard),
  ok.

%% @doc when the player puts 4 chips in a vertical row, wins
-spec wins_vertically(config()) -> ok.
wins_vertically(_Config) ->
  EmptyBoard = fiar_core:start(),
  CheckMateBoard = drop_chips([1, 2, 1, 2, 1, 2], EmptyBoard),
  won = fiar_core:play(1, CheckMateBoard),
  ok.

%% @doc when the player puts 4 chips in a horizontal row, wins
-spec wins_horizontally(config()) -> ok.
wins_horizontally(_Config) ->
  EmptyBoard = fiar_core:start(),
  CheckMateBoard = drop_chips([2, 5, 3, 6, 4, 7], EmptyBoard),
  won = fiar_core:play(1, CheckMateBoard),
  ok.

%% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_right_diagonally(config()) -> ok.
wins_right_diagonally(_Config) ->
  EmptyBoard = fiar_core:start(),
  CheckMateBoard = drop_chips([4, 4, 4, 4, 6, 3, 3, 3, 2, 2, 7], EmptyBoard),
  won = fiar_core:play(1, CheckMateBoard),
  ok.

%% @doc when the player puts 4 chips in a diagonal row, wins
-spec wins_left_diagonally(config()) -> ok.
wins_left_diagonally(_Config) ->
  EmptyBoard = fiar_core:start(),
  CheckMateBoard = drop_chips([4, 4, 4, 4, 2, 5, 5, 5, 6, 6, 1], EmptyBoard),
  won = fiar_core:play(7, CheckMateBoard),
  ok.

%% @private
%% @doc fills all columns in the board except #3
almost_fill_board(Board) ->
  WithColsExcept3 = lists:foldl(fun fill_column/2, Board, [1, 4, 2, 5, 6, 7]),
  drop_chips(6, 3, WithColsExcept3).

%% @private
fill_column(Col, Board) -> drop_chips(7, Col, Board).

%% @private
drop_chips(0, _Col, Board) -> Board;
drop_chips(N, Col, Board) ->
  {Col, Board, {next, NextBoard}} = {Col, Board, fiar_core:play(Col, Board)},
  drop_chips(N-1, Col, NextBoard).

%% @private
drop_chips([], Board) -> Board;
drop_chips([Col|Rest], Board) ->
  {Col, Board, {next, NextBoard}} = {Col, Board, fiar_core:play(Col, Board)},
  drop_chips(Rest, NextBoard).