%% @doc match model
-module(fiar_match).
-author('euen@inakanetworks.com').

-behaviour(sumo_doc).

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
    , sumo:new_field(state,         string,       [{length, 255}, not_null])
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
  Match =
    [ {player1,     Player1}
    , {player2,     Player2}
    , {state,       on_course}
    , {created_at,  Now}
    , {updated_at,  Now}],
  sumo:persist(match, Match).

-spec update(user(), map()) -> user().
update(User, Changes) ->

  Now = {datetime, calendar:universal_time()},
  Match =
    [ {_}
    , {_}
    , {state,       State}
    , {_}
    , {updated_at,  Now}],
  sumo:persist(match, Match).


  sumo:persist(thoughtz_users, UserUpdated).

get_id(Match) -> proplists:get_value(id, Match).

get_player1(Match) -> proplists:get_value(player1, Match).

get_player2(Match) -> proplists:get_value(player2, Match).

get_state(Match) -> proplists:get_value(state, Match).