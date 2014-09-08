-module (fiar).
-author('euen@inakanetworks.com').
-behaviour(application).

-type match() :: fiar_match:id().
-type player() :: fiar_match:player().
-type status() :: fiar_match_repo:status().

-export([start/0, stop/0, start/2, stop/1, start_match/2,
         play/2, match_status/1]).

-spec start() -> ok | {error, term()}.
start() ->
  ok = application:start(sasl),
  ok = application:start(syntax_tools),
  ok = application:start(compiler),
  ok = application:start(goldrush),
  ok = application:start(lager),
  ok = application:start(worker_pool),
  ok = application:start(crypto),
  ok = application:start(emysql),
  ok = application:start(sumo_db),
  sumo:create_schema(),
  application:start(fiar).

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(fiar),
  application:stop(sumo_db),
  application:stop(emysql),
  application:stop(crypto),
  application:stop(worker_pool),
  application:stop(lager),
  application:stop(goldrush),
  application:stop(compiler),
  application:stop(syntax_tools),
  application:stop(sasl).

-spec start_match(player(), player()) -> match().
start_match(Player1, Player2) ->
  fiar_match_repo:start(Player1, Player2).

-spec play(match(), fiar_core:col()) -> won | drawn | next.
play(Mid, Col) ->
  fiar_match_repo:play(Mid, Col).

-spec match_status(match()) -> status().
match_status(Match) ->
  fiar_match_repo:status(Match).

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

-spec start(atom(), any()) -> {ok, pid()} | {error, any()}.
start(normal, _Args) ->
  fiar_sup:start_link().