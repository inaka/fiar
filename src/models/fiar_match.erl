%% @doc match model
-module(fiar_match).
-author('euen@inakanetworks.com').

-behaviour(sumo_doc).

%%% Public API
-export(
  [ new/2
  , get_id/1
  , get_player1/1
  , get_player2/1
  , get_state/1
  , get_player/1
  , get_status/1
  , set_status/2
  , set_state/2
  , set_updated_at/1
  , to_json/1
  , match_list_to_json/1
  ]).
%%% Behaviour callbacks.
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,            integer,      [id, not_null, auto_increment])
    , sumo:new_field(player1,       integer,      [not_null])
    , sumo:new_field(player2,       integer,      [not_null])
    , sumo:new_field(status,        string,       [{length, 255}, not_null])
    , sumo:new_field(state,         binary,       [not_null])
    , sumo:new_field(created_at,    datetime,     [not_null])
    , sumo:new_field(updated_at,    datetime,     [not_null])
    ]).

sumo_sleep(Match) ->
  [ {id,          proplists:get_value(id, Match)}
  , {player1,     proplists:get_value(player1, Match)}
  , {player2,     proplists:get_value(player2, Match)}
  , {status,      atom_to_binary(proplists:get_value(status, Match), utf8)}
  , {state,       term_to_binary(proplists:get_value(state, Match))}
  , {created_at,  proplists:get_value(created_at, Match)}
  , {updated_at,  proplists:get_value(updated_at, Match)}
  ].

sumo_wakeup(Match) ->
  [ {id,          proplists:get_value(id, Match)}
  , {player1,     proplists:get_value(player1, Match)}
  , {player2,     proplists:get_value(player2, Match)}
  , {status,      binary_to_atom(proplists:get_value(status, Match), utf8)}
  , {state,       binary_to_term(proplists:get_value(state, Match))}
  , {created_at,  proplists:get_value(created_at, Match)}
  , {updated_at,  proplists:get_value(updated_at, Match)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates, stores and returns a news flash.
new(Player1, Player2) ->
  Now = {datetime, calendar:universal_time()},
  State = fiar_core:start(),
  [ {player1,     Player1}
  , {player2,     Player2}
  , {status,      on_course}
  , {state,       State}
  , {created_at,  Now}
  , {updated_at,  Now}].

get_id(Match) -> proplists:get_value(id, Match).

get_player(Match) ->
  State = get_state(Match),
  case fiar_core:get_current_chip(State) of
    1 -> get_player1(Match);
    2 -> get_player2(Match)
  end.

get_player1(Match) -> proplists:get_value(player1, Match).

get_player2(Match) -> proplists:get_value(player2, Match).

get_state(Match) -> proplists:get_value(state, Match).

get_status(Match) -> proplists:get_value(status, Match).

set_status(Match, Status) -> [{status, Status} | Match].

set_state(Match, State) -> [{state, State} | Match].

set_updated_at(Match) -> [{datetime, calendar:universal_time()} | Match].

match_list_to_json(Matches) ->
  lists:map(fun to_json/1, Matches).

to_json(Match) ->
  { [to_json_attr(K, V) || {K, V} <- Match] }.

to_json_attr(status, Atom) -> {status, atom_to_binary(Atom, utf8)};
to_json_attr(K, {datetime, DT}) -> {K, datetime_to_json(DT)};
to_json_attr(state, BoardState) -> {state, fiar_core:to_json(BoardState)};
to_json_attr(K, V) -> {K, V}.

-spec datetime_to_json(choosy_utils:datetime()) ->
  binary().
%% @doc Converts a datetime record into a binary representation of its data.
datetime_to_json({{Yi,Mi,Di},{Hi,Ni,Si}}) ->
  Y = integer_to_list(Yi),
  M = integer_to_list(Mi),
  D = integer_to_list(Di),
  H = integer_to_list(Hi),
  N = integer_to_list(Ni),
  S = integer_to_list(Si),
  iolist_to_binary([Y,"-",M,"-",D,"T",H,":",N,":",S,".000000Z"]).