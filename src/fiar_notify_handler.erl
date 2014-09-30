-module (fiar_notify_handler).
-behavior(lasse_handler).

-export([ init/3
        , handle_notify/2
        , handle_info/2
        , handle_error/3
        , terminate/3
        ]).

init(_InitArgs, _LastEventId, Req) ->
  try
    case fiar_auth:check_auth(Req) of
      {authenticated, User, Req1} ->
        {MatchId, Req2} =  cowboy_req:binding(match_id, Req1),
        UserId = fiar_user:get_id(User),
        fiar:get_match(MatchId, User),
        Name = notifier_process_name(MatchId, UserId),
        Process = list_to_atom(Name),
        true = process_register(Process),
        {ok, Req2, #{user => User}};
      {not_authenticated, _AuthHeader, Req1} ->
        {shutdown, 401, [], [], Req1, #{}}
    end
  catch
    _:Exception -> 
      lager:warning("Exception in lasse_handler GET: ~p~n", [Exception]),
      {shutdown, 404, [], [], Req, #{}}
  end.

handle_notify(Msg, State) ->
    {send, [{data, Msg}, {name, <<"turn">>}], State}.

handle_info(_Msg, State) ->
    {nosend, State}.

handle_error(_Msg, _Reason, State) ->
    State.

terminate(_Reason, _Req, _State) ->
  ok.

notifier_process_name(MatchId, UserId) ->
  lists:flatten(io_lib:format("fiar_player_~s_~p", [MatchId, UserId])).

process_register(Process) ->
  try erlang:register(Process, self())
  catch
    _ -> erlang:register(Process, self())
  end.