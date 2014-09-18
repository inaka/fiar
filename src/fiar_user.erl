%% @doc user model
-module (fiar_user).
-author('euen@inakanetworks.com').

-behaviour(sumo_doc).

%%% Public API
-export(
  [ new/1
  , get_id/1
  , get_username/1
  , get_key/1
  , set_username/2
  , set_key/2
  , set_updated_at/1
  , to_json/1
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
    , sumo:new_field(key,           string,       [ {length, 255}
                                                  , not_null
                                                  ])
    , sumo:new_field(created_at,    datetime,     [not_null])
    , sumo:new_field(updated_at,    datetime,     [not_null])
    ]).

sumo_sleep(User) ->
  [ {id,          proplists:get_value(id, User)}
  , {username,    proplists:get_value(username, User)}
  , {key,         proplists:get_value(key, User)}
  , {created_at,  proplists:get_value(created_at, User)}
  , {updated_at,  proplists:get_value(updated_at, User)}
  ].

sumo_wakeup(User) ->
  [ {id,          proplists:get_value(id, User)}
  , {username,    proplists:get_value(username, User)}
  , {key,         proplists:get_value(key, User)}
  , {created_at,  proplists:get_value(created_at, User)}
  , {updated_at,  proplists:get_value(updated_at, User)}
  ].

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% PUBLIC API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Creates, stores and returns a news flash.
new(Username) ->
  Now = {datetime, calendar:universal_time()},
  Key = ktn_random:generate(),
  [ {username,    Username}
  , {key,         Key}
  , {created_at,  Now}
  , {updated_at,  Now}].

get_id(User) -> proplists:get_value(id, User).

get_username(User) -> proplists:get_value(username, User).

get_key(User) -> proplists:get_value(key, User).

set_username(User, Username) -> [{status, Username} | User].

set_key(User, Key) -> [{status, Key} | User].

set_updated_at(User) -> [{datetime, calendar:universal_time()} | User].

to_json(User) ->
  { [to_json_attr(K, V) || {K, V} <- User] }.

to_json_attr(K, {datetime, DT}) -> {K, datetime_to_json(DT)};
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