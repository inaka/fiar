%% @doc match model
-module(fiar_match).
-author('euen@inakanetworks.com').

-behaviour(sumo_doc).

-record(state, {board::fiar_core:board(), next_chip = 1 :: fiar_core:chip()}).

%%% Public API
-export(
  [ new/0
  , get_id/1
  , get_player1/1
  , get_player2/1
  , get_state/1
  ]).
%%% Behaviour callbacks.
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,            integer,      [id, not_null, auto_increment])
    , sumo:new_field(player1,       string,       [{length, 255}, not_null])
    , sumo:new_field(player2,       string,       [{length, 255}, not_null])
    , sumo:new_field(status,        string,       [{length, 255}, not_null])
    , sumo:new_field(state,         binary,       [{length, 255}, not_null])
    , sumo:new_field(created_at,    datetime,     [not_null])
    , sumo:new_field(updated_at,    datetime,     [not_null])
    ]).

sumo_sleep(Match) -> Match.

sumo_wakeup(Match) -> Match.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates, stores and returns a news flash.
new(Player1, Player2) ->
  Now = {datetime, calendar:universal_time()},
  State = #state{board = {[], [], [], [], [], [], []}},
  Match =
    [ {player1,     Player1}
    , {player2,     Player2}
    , {status,      on_course}
    , {state,       State}
    , {created_at,  Now}
    , {updated_at,  Now}].

get_id(Match) -> proplists:get_value(id, Match).

get_player1(Match) -> proplists:get_value(player1, Match).

get_player2(Match) -> proplists:get_value(player2, Match).

get_state(Match) -> proplists:get_value(state, Match).

get_status(Match) -> proplists:get_value(status, Match).

set_status(Match, Status) ->
  [[{X,case X of status -> Status; _ -> Y end} || {X,Y} <- Z] || Z <- Match].

set_state(Match, State) ->
  [[{X,case X of status -> State; _ -> Y end} || {X,Y} <- Z] || Z <- Match].

set_updated_at(Match) ->
  Now = {datetime, calendar:universal_time()},
  [[{X,case X of updated_at -> Now; _ -> Y end} || {X,Y} <- Z] || Z <- Match].