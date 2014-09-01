-module (fiar).
-author('euen@inakanetworks.net').
-behaviour(application).

-type chip() :: 1|2.
-type column() :: [chip()].
-type board() ::
  {column(), column(), column(), column(), column(), column(), column()}.
-type col() :: 1..7.
-type from() :: {pid(), _}.
-type reason() :: normal | shutdown | {shutdown, term()} | term().
-type oldVsn() :: term() | {down, term()}.
-type match() :: pid().
-export_type([chip/0, board/0, col/0]).
-export_type([from/0, reason/0, oldVsn/0]).


-export([start/2, stop/1, start_match/0,
        play/2, stop_match/1]).

-spec start(atom(), any()) -> ok.
start(normal, _Args) ->
    fiar_sup:start_link().

-spec stop(_) -> ok.
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