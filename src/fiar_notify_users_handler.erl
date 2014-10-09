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
  Pids = pg2:get_members(fiar_connected_users),
  lists:foreach(
    fun(Pid) ->
      lasse_handler:notify(Pid, {EventName, Content})
    end, Pids).

init(_InitArgs, _LastEventId, Req) ->
  try
    case fiar_auth:check_auth(Req) of
      {authenticated, User, Req1} ->
        process_register(fiar_user:get_id(User)),
        pg2:create(fiar_connected_users),
        pg2:join(fiar_connected_users, self()),
        ConnectedUsers = get_connected_users(),
        FirstEvent = [{data, jiffy:encode(ConnectedUsers)}],
        {ok, Req, [FirstEvent], #{user => User}};
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
  case is_mine(Match, maps:get(user, State)) of
    true -> erlang:put(current_match, fiar_match:get_id(Match));
    false -> ok
  end,
  MatchJson = jiffy:encode(fiar_match:to_json(Match)),
  {send, [{data, MatchJson}, {name, <<"match_started">>}], State};
handle_notify({match_ended, Match}, State) ->
  case is_mine(Match, maps:get(user, State)) of
    true -> erlang:erase(current_match);
    false -> ok
  end,
  MatchJson = jiffy:encode(fiar_match:to_json(Match)),
  {send, [{data, MatchJson}, {name, <<"match_ended">>}], State}.

handle_info(stop, State) ->
  {stop, State};
handle_info(_Msg, State) ->
  {nosend, State}.

handle_error(_Msg, _Reason, State) ->
  State.

terminate(_Reason, _Req, State) ->
  broadcast(user_disconected, maps:get(user, State)),
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

get_connected_users() ->
  Pids = pg2:get_members(fiar_connected_users),
  [get_connected_user(Pid) || Pid <- Pids].

get_connected_user(Pid) ->
  [{registered_name, Name}, {dictionary, Dict}] =
    erlang:process_info(Pid, [registered_name, dictionary]),
  User = process_name_to_user(Name),
  CurrentMatchId = 
    case proplists:get_value(current_match, Dict) of
      undefined -> [];
      Val -> Val
    end,
  User1 = fiar_user:to_json_no_pass(User),
  {[{user, User1}, {current_match, CurrentMatchId}]}.

is_mine(Match, User) ->
  UserId = fiar_user:get_id(User),
  try UserId = fiar_match:get_player(Match) of
    _ -> true
  catch
    _ -> false
  end.

process_name_to_user(Proc) ->
  "fiar_user_" ++ UserIdStr = atom_to_list(Proc),
  UserId = list_to_integer(UserIdStr),
  fiar:find_user(UserId).