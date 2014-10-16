-module (fiar_notify_users_handler).
-behavior(lasse_handler).

-export([ init/3
        , handle_notify/2
        , handle_info/2
        , handle_error/3
        , terminate/3
        ]).
-export([ broadcast/2
        , setup/0
        ]).

setup() ->
  pg2:create(fiar_connected_users).

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
        pg2:join(fiar_connected_users, self()),
        ConnectedUsers = get_connected_users(),
        FirstEvent = [{data, jiffy:encode(ConnectedUsers)}],
        fiar:send_event(fiar_user, connected, [User]),
        {ok, Req, [FirstEvent], #{user => User}};
      {not_authenticated, _AuthHeader, Req1} ->
        {shutdown, 401, [], [], Req1, #{}}
    end
  catch
    _:notfound -> 
      {shutdown, 404, [], [], Req, #{}}
  end.

handle_notify({user_conected, User}, State) ->
  UserJson = jiffy:encode(fiar_user:to_json(User, public)),
  {send, [{data, UserJson}, {name, <<"user_conected">>}], State};
handle_notify({user_disconnected, User}, State) ->
  UserJson = jiffy:encode(fiar_user:to_json(User, public)),
  {send, [{data, UserJson}, {name, <<"user_disconnected">>}], State};
handle_notify({match_started, Match}, State) ->
  MatchJson = jiffy:encode(fiar_match:to_json(Match)),
  {send, [{data, MatchJson}, {name, <<"match_started">>}], State};
handle_notify({match_ended, Match}, State) ->
  MatchJson = jiffy:encode(fiar_match:to_json(Match)),
  {send, [{data, MatchJson}, {name, <<"match_ended">>}], State}.

handle_info(stop, State) ->
  {stop, State};
handle_info(_Msg, State) ->
  {nosend, State}.

handle_error(_Msg, _Reason, State) ->
  State.

terminate(_Reason, _Req, #{user := User}) ->
  fiar:send_event(fiar_user, disconnected, [User]),
  ok;
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

get_connected_users() ->
  Pids = pg2:get_members(fiar_connected_users),
  [get_connected_user(Pid) || Pid <- Pids].

get_connected_user(Pid) ->
  [{registered_name, Name}] =
    erlang:process_info(Pid, [registered_name]),
  User = process_name_to_user(Name),
  User1 = fiar_user:to_json(User, public),
  CurrentMatches = fiar:current_matches(User),
  CurrentMatchesJson = fiar_match:matches_to_json(CurrentMatches),
  Response = [{user, User1}, {current_matches, CurrentMatchesJson}],
  {Response}.

process_name_to_user(Proc) ->
  "fiar_user_" ++ UserIdStr = atom_to_list(Proc),
  UserId = list_to_integer(UserIdStr),
  fiar:find_user(UserId).