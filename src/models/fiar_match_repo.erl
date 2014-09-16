-module (fiar_match_repo).

-type status() :: {won_by, fiar_match:player()}
                | drawn
                | {next_player, fiar_match:player()}.
-export_type([status/0]).

-export([start/2, get_match/1, play/2, status/1, get_matches/0]).

start(Player1, Player2) ->
  Match = fiar_match:new(Player1, Player2),
  lager:info("Match Previous to save: ~p", [Match]),
  StoredMatch = sumo:persist(fiar_match, Match),
  fiar_match:get_id(StoredMatch).

play(Mid, Col) ->
  Match = get_match(Mid),
  Status = fiar_match:get_status(Match),
  case Status of
    on_course -> 
      try
        State = fiar_match:get_state(Match),
        {Reply, NewStatus, NewState} =
          case fiar_core:play(Col, State) of
            {Result, St} -> {Result, Status, St};
            Result -> {Result, new_status(Result, State), State}
          end,
          NewMatch0 = fiar_match:set_status(Match, NewStatus),
          NewMatch2 = fiar_match:set_state(NewMatch0, NewState),
          NewMatch3 = fiar_match:set_updated_at(NewMatch2),
          sumo:persist(fiar_match, NewMatch3),
          Reply
      catch
        _:Ex -> throw({error, Ex})
      end;
    Status -> throw({match_finished, Status})
  end.

status(Mid) ->
  Match = 
    case sumo:find(fiar_match, Mid) of
      notfound -> throw({notfound, Mid});
      M -> M
    end,
  Status = fiar_match:get_status(Match),
  case Status of
    on_course -> {next_player, fiar_match:get_player(Match)};
    won_by_player1 -> {won_by, fiar_match:get_player1(Match)};
    won_by_player2 -> {won_by, fiar_match:get_player2(Match)};
    drawn -> drawn;
    Status -> throw({invalid_status, Status})
  end.

%% @private
new_status(won, State) ->
  case fiar_core:get_current_chip(State) of
    1 -> won_by_player1;
    2 -> won_by_player2
  end;
new_status(drawn, _State) -> drawn.

get_match(Mid) ->
  case sumo:find(fiar_match, Mid) of
    notfound -> throw({notfound, Mid});
    M -> M
  end.

get_matches() ->
  sumo:find_all(fiar_match).