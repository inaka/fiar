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
        ProcessName = fiar_utils:process_name(MatchId, UserId),
        process_register(ProcessName),
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

handle_info(stop, State) ->
    {stop, State};
handle_info(_Msg, State) ->
    {nosend, State}.

handle_error(_Msg, _Reason, State) ->
    State.

terminate(_Reason, _Req, _State) ->
  ok.

process_register(Process) ->
  case whereis(Process) of
      undefined -> ok;
      _ ->
      Process ! stop,
      erlang:unregister(Process)
  end,
  erlang:register(Process, self()).