-module (fiar_match_repo).

start(Player1, Player2) ->
  Match = fiar_match:new(Player1, Player2),
  StoredMatch = sumo:persist(fiar_match, Match),
  fiar_match:get_id(StoredMatch).

play(Mid, Col) ->
  Match = 
    case sumo:find(fiar_match, Mid) of
      [_] -> M;
      notfound -> throw({notfound, Mid})
    end,
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

new_status(won, State) ->
  case fiar_core:get_current_chip(State) of
    1 -> won_by_player1,
    2 -> won_by_player2
  end;
new_status(drawn, _State) -> drawn.