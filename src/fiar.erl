-module (fiar).
-author('euen@inakanetworks.com').
-behaviour(application).

-type match() :: pid().

-export([start/0, stop/0, start/2, stop/1, start_match/0,
         play/2, stop_match/1]).

-spec start() -> ok | {error, term()}.
start() ->
  application:start(fiar).

-spec start(atom(), any()) -> {ok, pid()} | {error, any()}.
start(normal, _Args) ->
  fiar_sup:start_link().

 -spec stop() -> ok | {error, term()}.
stop() ->
  application:stop(fiar).

-spec stop(atom()) -> ok.
stop(_State) ->
  ok.

-spec start_match() -> match().
start_match() ->
  {ok, Pid} = fiar_sup:start_match(),
  Pid.

-spec play(match(), fiar_core:col()) -> won | drawn | next.
play(Match, Col) ->
  fiar_match:play(Match, Col).

-spec stop_match(match()) -> ok.
stop_match(Match) ->
  fiar_match:stop(Match).