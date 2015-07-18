-module (fiar_notify_handler).
-behavior(lasse_handler).

-export([ init/3
        , handle_notify/2
        , handle_info/2
        , handle_error/3
        , terminate/3
        ]).
-export([ notify/4
        ]).

notify(EventName, MatchId, UserId, Match) ->
  ProcessName = process_name(MatchId, UserId),
  lager:info("Event: ~p sent to ~p.", [EventName, ProcessName]),
  try lasse_handler:notify(ProcessName, {EventName, Match})
  catch
    _:badarg ->
      lager:warning(
        "Could not deliver notification:~nStack: ~p",
        [erlang:get_stacktrace()]),
      ok %% the destination process doesn't exist
  end.

init(_InitArgs, _LastEventId, Req) ->
  try
    case fiar_auth:check_auth(Req) of
      {authenticated, User, Req1} ->
        {MatchIdBin, Req2} =  cowboy_req:binding(match_id, Req1),
        MatchId = binary_to_integer(MatchIdBin),
        UserId = fiar_user:get_id(User),
        fiar:get_match(MatchId, User),
        process_register(MatchId, UserId),
        {ok, Req2, #{user => User}};
      {not_authenticated, _AuthHeader, Req1} ->
        {shutdown, 401, [], [], Req1}
    end
  catch
    _:notfound ->
      {shutdown, 404, [], [], Req}
  end.

handle_notify({match_updated, Match}, State) ->
  MatchJson = jiffy:encode(fiar_match:to_json(Match)),
  {send, [{data, <<MatchJson>>}, {name, <<"turn">>}], State};
handle_notify({match_deleted, Match}, State) ->
  MatchJson = jiffy:encode(fiar_match:to_json(Match)),
  {send, [{data, <<MatchJson>>}, {name, <<"match_ended">>}], State}.

handle_info(stop, State) ->
  {stop, State};
handle_info(_Msg, State) ->
  {nosend, State}.

handle_error(_Msg, _Reason, State) ->
  State.

terminate(_Reason, _Req, _State) ->
  ok.

process_register(MatchId, UserId) ->
  Process = process_name(MatchId, UserId),
  case whereis(Process) of
      undefined -> ok;
      _ ->
      Process ! stop,
      erlang:unregister(Process)
  end,
  erlang:register(Process, self()).

process_name(MatchId, UserId) ->
  list_to_atom(  "fiar_player_"
              ++ integer_to_list(MatchId)
              ++ "_"
              ++ integer_to_list(UserId)).