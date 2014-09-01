-module(fiar_core).
-author('euen@inakanetworks.net').
-export([start/0, play/2]).
-record(state, {board::fiar:board(), next_chip = 1 :: fiar:chip()}).
-type state() :: #state{}.

-spec start() -> fiar:state().
start()  -> 
  #state{board = {[], [], [], [], [], [], []}}.

-spec play(fiar:col(), state()) -> won | drawn | {next, state()}.
play(Col, _) when Col < 1 orelse Col > 7 ->
  throw(invalid_column);
play(Col, State = #state{board = Board, next_chip = NextChip}) ->
  Column = element(Col, Board),
  case Column of
    [_, _, _, _, _, _, _] -> throw(invalid_column);
    [NextChip, NextChip, NextChip | _] -> won;
    _ ->
      NewColumn = [NextChip|Column],
      NewBoard = setelement(Col, Board, NewColumn),
      case analyze(Col, NewColumn, NextChip, NewBoard) of
        next ->
          NewState = State#state{board = NewBoard,
                     next_chip = diff_chip(NextChip)},
          {next, NewState};
        won -> won;
        drawn -> drawn
      end
  end.

diff_chip(1) -> 2;
diff_chip(2) -> 1.

analyze(Col, Column, Chip, Board) ->
  RowNum = length(Column),
  case wins_row(RowNum, Chip, Board) orelse
       wins_left_diag(Col, RowNum, Chip, Board) orelse
       wins_right_diag(Col, RowNum, Chip, Board) of
          true -> won;
          false ->
            case is_full(Board) of
                true -> drawn;
                false -> next
            end
  end.

wins_row(RowNum, Chip, Board) ->
  Row = get_row(RowNum, Board),
  contains_four(Chip, Row).
 
contains_four(_Chip, List) when length(List) < 4 -> false;
contains_four(Chip, [Chip, Chip, Chip, Chip | _ ])  -> true;
contains_four(Chip, [ _ | Rest ]) -> contains_four(Chip, Rest).

get_row(RowNum, Board) ->
  Columns = tuple_to_list(Board),
  lists:map(fun(Column) -> get_chip(RowNum, Column) end, Columns).

get_left_diag(Col, RowNum, Board) ->
  UpLeftDiag = get_up_left_diag(Col, RowNum, Board, []),
  DownLeftDiag = get_down_left_diag(Col, RowNum, Board, []),
  UpLeftDiag ++ tl(lists:reverse(DownLeftDiag)).

get_up_left_diag(Col, RowNum, Board, Acc) when Col =:= 1; RowNum =:= 7 ->
  Chip = get_chip(Col, RowNum, Board),
  [Chip | Acc];
get_up_left_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_up_left_diag(Col-1, RowNum+1, Board, Next).

get_down_left_diag(7, RowNum, Board, Acc) ->
  Chip = get_chip(7, RowNum, Board),
  [Chip | Acc];
get_down_left_diag(Col, 1, Board, Acc) -> 
  Chip = get_chip(Col, 1, Board),
  [Chip | Acc];
get_down_left_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_down_left_diag(Col+1, RowNum-1, Board, Next).

get_right_diag(Col, RowNum, Board) ->
  DownRightDiag = get_down_right_diag(Col, RowNum, Board, []),
  UpRightDiag = get_up_right_diag(Col, RowNum, Board, []),
  DownRightDiag ++ tl(lists:reverse(UpRightDiag)).

get_down_right_diag(Col, RowNum, Board, Acc) when Col =:= 1; RowNum =:= 1 ->
  Chip = get_chip(Col, RowNum, Board),
  [Chip | Acc];
get_down_right_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_down_right_diag(Col-1, RowNum-1, Board, Next).

get_up_right_diag(Col, RowNum, Board, Acc) when Col =:= 7; RowNum =:= 7 ->
  Chip = get_chip(Col, RowNum, Board),
  [Chip | Acc];
get_up_right_diag(Col, RowNum, Board, Acc) ->
  Chip = get_chip(Col, RowNum, Board),
  Next = [Chip | Acc],
  get_up_right_diag(Col+1, RowNum+1, Board, Next).

get_chip(RowNum, Column) when length(Column) >= RowNum ->
  lists:nth(RowNum, lists:reverse(Column));
get_chip(_RowNum, _Column) -> 0.

get_chip(Col, RowNum, Board) ->
  Columns = tuple_to_list(Board),
  Column = lists:nth(Col, Columns),
  get_chip(RowNum, Column).

wins_left_diag(Col, RowNum, Chip, Board) ->
  Diag = get_left_diag(Col, RowNum, Board),
  contains_four(Chip, Diag).

wins_right_diag(Col, RowNum, Chip, Board) ->
  Diag = get_right_diag(Col, RowNum, Board),
  contains_four(Chip, Diag).

is_full(Board) ->
  Columns = tuple_to_list(Board),
  Fun = fun(Col) ->
         case length(Col) of
               7 -> true;
               _ -> false
          end
  end,
  lists:all(Fun, Columns).