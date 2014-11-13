-module (fiar).
-author('euen@inakanetworks.com').
-behaviour(application).

-type match() :: fiar_match:id().
-type user() :: fiar_user_repo:user().
-type player() :: fiar_match:player().
-type username() :: fiar_user:username().
-type pass() :: fiar_user:pass().

-export([ start/0
        , stop/0
        , start/2
        , stop/1
        , start_match/2
        , play/3
        , get_match/2
        , get_matches/1
        , new_user/2
        , find_user/1
        , notify/4
        , send_event/3
        , broadcast/2
        , delete_match/2
        , current_matches/1
        ]).

-spec start() -> ok | {error, term()}.
start() ->
  {ok, _} = application:ensure_all_started(fiar),
  sumo:create_schema().

-spec stop() -> ok | {error, term()}.
stop() -> application:stop(fiar).

-spec start(atom(), any()) -> {ok, pid()} | {error, any()}.
start(normal, _Args) ->
  {ok, Pid} = fiar_sup:start_link(),
  start_cowboy_listeners(),
  fiar_notify_users_handler:setup(),
  {ok, Pid}.

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

-spec start_match(player(), player()) -> fiar_match:match().
start_match(User1, User2) ->
  fiar_match_repo:start(User1, User2).

-spec new_user(username(), pass()) -> user().
new_user(Username, Pass) ->
  fiar_user_repo:create(Username, Pass).

-spec play(match(), fiar_core:col(), fiar_user:user()) -> won | drawn | next.
play(Mid, Col, User) ->
  try
    fiar_match_repo:play(Mid, Col, User)
  catch
    Ex -> throw(Ex)
  end.

get_match(MatchId, User) ->
  fiar_match_repo:get_match(MatchId, User).

get_matches(User) ->
  fiar_match_repo:get_matches(User).

find_user(UserId) when is_integer(UserId) ->
  fiar_user_repo:find_by_id(UserId);
find_user(Username) ->
  fiar_user_repo:find_by_username(Username).

notify(EventName, MatchId, UserId, Match) ->
  fiar_notify_handler:notify(EventName, MatchId, UserId, Match).

send_event(Module, EventName, Content) ->
  fiar_events:notify(Module, EventName, Content).

broadcast(EventName, Content) ->
  fiar_notify_users_handler:broadcast(EventName, Content).

delete_match(MatchId, User) ->
  fiar_match_repo:delete_match(MatchId, User).

current_matches(User) ->
  fiar_match_repo:current_matches(User).

start_cowboy_listeners() ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/matches", fiar_matches_handler, []},
           {"/matches/:match_id", fiar_single_match_handler, []},
           {"/users", fiar_users_handler, []},
           {"/me", fiar_current_user_handler, []},
           {<<"/">>, cowboy_static, {file, "./priv/static/index.html"}},
           {"/priv/static/[...]", cowboy_static, {dir, "priv/static/"}},
           { "/matches/:match_id/events", lasse_handler, [fiar_notify_handler]},
           { "/events", lasse_handler, [fiar_notify_users_handler]}
          ]
    }
  ]),
  cowboy:start_http(fiar_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ).
