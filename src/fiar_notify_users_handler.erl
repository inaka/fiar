-module (fiar_notify_users_handler).
-behavior(lasse_handler).

-export([ init/3
        , handle_notify/2
        , handle_info/2
        , handle_error/3
        , terminate/3
        ]).
-export([ broadcast/2
        ]).

broadcast(EventName, Content) ->
  Pids = pg2:get_all_members(fiar_users_online),
  lists:foreach(
    fun(Pid) ->
      lasse_handler:notify(Pid, {EventName, Content})
    end, Pids).

init(_InitArgs, _LastEventId, Req) ->
  try
    case fiar_auth:check_auth(Req) of
      {authenticated, User, Req1} ->
        process_register(fiar_user:get_id(User)),
        pg2:create(fiar_users_online),
        pg2:join(fiar_users_online, self()),
        {ok, Req, #{user => User}};
      {not_authenticated, _AuthHeader, Req1} ->
        {shutdown, 401, [], [], Req1, #{}}
    end
  catch
    _:notfound -> 
      {shutdown, 404, [], [], Req, #{}}
  end.

handle_notify({user_conected, User}, State) ->
  UserJson = jiffy:encode(fiar_user:to_json_no_pass(User)),
  {send, [{data, UserJson}, {name, <<"user_conected">>}], State};
handle_notify({user_disconected, User}, State) ->
  UserJson = jiffy:encode(fiar_user:to_json_no_pass(User)),
  {send, [{data, UserJson}, {name, <<"user_disconected">>}], State};
handle_notify({match_started, Match}, State) ->
  MatchJson = jiffy:encode(fiar_match:to_json(Match)),
  {send, [{data, MatchJson}, {name, <<"match_started">>}], State};
handle_notify({match_ended, Match}, State) ->
  MatchJson = jiffy:encode(fiar_user:to_json(Match)),
  {send, [{data, MatchJson}, {name, <<"match_ended">>}], State}.

handle_info(stop, State) ->
  {stop, State};
handle_info(_Msg, State) ->
  {nosend, State}.

handle_error(_Msg, _Reason, State) ->
  State.

terminate(_Reason, _Req, _State) ->
  ok.

%% @private
process_register(UserId) ->
  Process = process_name(UserId),
  case whereis(Process) of
      undefined -> ok;
      _ ->
      Process ! stop,
      erlang:unregister(Process)
  end,
  erlang:register(Process, self()).

process_name(UserId) ->
  list_to_atom("fiar_user_" ++ integer_to_list(UserId)).