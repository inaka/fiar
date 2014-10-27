%% @doc user model
-module (fiar_user).
-author('euen@inakanetworks.com').

-behaviour(sumo_doc).

%%% Public API
-export(
  [ new/2
  , get_id/1
  , to_json/2
  ]).
%%% Behaviour callbacks.
-export([sumo_schema/0, sumo_wakeup/1, sumo_sleep/1]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% BEHAVIOUR CALLBACKS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
sumo_schema() ->
  sumo:new_schema(?MODULE,
    [ sumo:new_field(id,            integer,      [ id
                                                  , not_null
                                                  , auto_increment
                                                  ])
    , sumo:new_field(username,      string,       [ {length, 255}
                                                  , not_null
                                                  , unique
                                                  ])
    , sumo:new_field(pass,           string,      [ {length, 255}
                                                  , not_null
                                                  ])
    , sumo:new_field(created_at,    datetime,     [not_null])
    , sumo:new_field(updated_at,    datetime,     [not_null])
    ]).

sumo_sleep(User) ->
  [ {id,          proplists:get_value(id, User)}
  , {username,    proplists:get_value(username, User)}
  , {pass,        proplists:get_value(pass, User)}
  , {created_at,  proplists:get_value(created_at, User)}
  , {updated_at,  proplists:get_value(updated_at, User)}
  ].

sumo_wakeup(User) ->
  [ {id,          proplists:get_value(id, User)}
  , {username,    proplists:get_value(username, User)}
  , {pass,        proplists:get_value(pass, User)}
  , {created_at,  proplists:get_value(created_at, User)}
  , {updated_at,  proplists:get_value(updated_at, User)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates, stores and returns a news flash.
new(Username, Pass) ->
  Now = {datetime, calendar:universal_time()},
  [ {username,    Username}
  , {pass,        Pass}
  , {created_at,  Now}
  , {updated_at,  Now}].

get_id(User) -> proplists:get_value(id, User).

to_json(User, private) ->
  { [to_json_attr(K, V) || {K, V} <- User] };
to_json(User, public) ->
  { [to_json_attr(K, V) || {K, V} <- User, K /= pass] }.

to_json_attr(K, {datetime, DT}) -> {K, fiar_utils:datetime_to_json(DT)};
to_json_attr(K, V) -> {K, V}.