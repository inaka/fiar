-module (fiar).
-author('euen@inakanetworks.com').
-behaviour(application).

-type match() :: fiar_match:id().
-type player() :: fiar_match:player().

-export([start/0, stop/0, start/2, stop/1, start_match/2,
         play/2, stop_match/1]).

-spec start() -> ok | {error, term()}.
start() ->
  application:start(fiar).

-spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(fiar).

-spec start_match(player(), player()) -> match().
start_match(Player1, Player2) ->
  fiar_match_repo:start(Player1, Player2).

-spec play(match(), fiar_core:col()) -> won | drawn | next.
play(Mid, Col) ->
  fiar_match_repo:play(Mid, Col).
  
-spec stop_match(match()) -> ok.
stop_match(Match) ->
  fiar_match_repo:stop(Match).

match_status(Match) ->
  fiar_match:get_status(Match).

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

-spec start(atom(), any()) -> {ok, pid()} | {error, any()}.
start(normal, _Args) ->
  fiar_sup:start_link().