-module (fiar).
-author('euen@inakanetworks.com').
-behaviour(application).

-type match() :: fiar_match:id().
-type user() :: fiar_user_repo:user().
-type player() :: fiar_match:player().
-type username() :: fiar_user:username().

-export([ start/0
        , stop/0
        , start/2
        , stop/1
        , start_match/2
        , play/3
        , get_match/2
        , get_matches/1
        , new_user/1
        , find_user/1
        ]).

-spec start() -> ok | {error, term()}.
start() ->
  application:ensure_all_started(fiar),
  sumo:create_schema().

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(fiar),
  application:stop(cowboy),
  application:stop(ranch),
  application:stop(cowlib),
  application:stop(sumo_db),
  application:stop(emysql),
  application:stop(crypto),
  application:stop(worker_pool),
  application:stop(lager),
  application:stop(goldrush),
  application:stop(compiler),
  application:stop(syntax_tools),
  application:stop(sasl).

-spec start(atom(), any()) -> {ok, pid()} | {error, any()}.
start(normal, _Args) ->
  {ok, Pid} = fiar_sup:start_link(),
  start_cowboy_listeners(),
  {ok, Pid}.

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

-spec start_match(player(), player()) -> match().
start_match(User1, User2) ->
  fiar_match_repo:start(User1, User2).

-spec new_user(username()) -> user().
new_user(Username) ->
  fiar_user_repo:create(Username).

-spec play(match(), fiar_core:col(), fiar_user:user()) -> won | drawn | next.
play(Mid, Col, User) ->
  try
    Result = fiar_match_repo:play(Mid, Col, User),
    lager:info("result: ~p", [Result]),
    Result
  catch
    Ex -> throw(Ex)
  end.

get_match(MatchId, User) ->
  fiar_match_repo:get_match(MatchId, User).

get_matches(User) ->
  fiar_match_repo:get_matches(User).

find_by_username(Username) ->
  case fiar_user_repo:find_by_username(Username) of
    notfound ->
          throw(bad_request);
    User -> 
          User
  end.

start_cowboy_listeners() ->
  Dispatch = cowboy_router:compile([
    {'_', [{"/matches", fiar_matches_handler, []},
           {"/matches/:match_id", fiar_single_match_handler, []},
           {"/users", fiar_users_handler, []}]}
  ]),
  cowboy:start_http(fiar_http_listener, 100, [{port, 8080}],
    [{env, [{dispatch, Dispatch}]}]
  ).
