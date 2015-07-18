-module(fiar_repo_mysql).
-author('euen@inakanetworks.com').

-behaviour(sumo_repo).

%%% General repo functions.
-export(
  [ init/1
  , create_schema/2
  , delete/3
  , delete_all/2
  , delete_by/3
  , execute/2
  , execute/3
  , find_all/2
  , find_all/5
  , find_by/3
  , find_by/5
  , persist/2
  ]).

-export([fiar_delete/3]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% sumo_repo functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% should be sumo_repo_mysql:state(), we miss the -extends directive
-type state() :: term(). 

-spec init(term()) -> {ok, state()}.
-spec persist(sumo_internal:doc(), state()) -> 
            sumo_repo:result(sumo_internal:doc(), state()).
-spec delete(sumo:schema_name(), term(), state()) ->
            sumo_repo:result(sumo_repo:affected_rows(), state()).
-spec delete_by(sumo:schema_name(), sumo:conditions(), state()) ->
            sumo_repo:result(sumo_repo:affected_rows(), state()).
-spec delete_all(sumo:schema_name(), state()) ->
            sumo_repo:result(sumo_repo:affected_rows(), state()).
-spec find_by(sumo:schema_name(), sumo:conditions(), state()) ->
            sumo_repo:result([sumo_internal:doc()], state()).
-spec find_by(sumo:schema_name(), sumo:conditions(), non_neg_integer(),
                  non_neg_integer(), state()) ->
            sumo_repo:result([sumo_internal:doc()], state()).
-spec create_schema(sumo:schema(), state()) -> sumo_repo:result(state()).

init(Options) ->
  sumo_repo_mysql:init(Options).

create_schema(Schema, State) ->
 sumo_repo_mysql:create_schema(Schema, State).

delete(DocName, Id, State) ->
  sumo_repo_mysql:delete(DocName, Id, State).

delete_all(DocName, State) ->
  sumo_repo_mysql:delete_all(DocName, State).

delete_by(DocName, Conditions, State) ->
  sumo_repo_mysql:delete_by(DocName, Conditions, State).

execute(Query, State) ->
  sumo_repo_mysql:execute(Query, State).

execute(Query, Args, State) ->
  sumo_repo_mysql:execute(Query, Args, State).

find_all(DocName, State) ->
  sumo_repo_mysql:find_all(DocName, State).

find_all(DocName, OrderField, Limit, Offset, State) ->
  sumo_repo_mysql:find_all(DocName, OrderField, Limit, Offset, State).

find_by(DocName, Conditions, State) ->
  sumo_repo_mysql:find_by(DocName, Conditions, State).

find_by(DocName, Conditions, Limit, Offset, State) ->
  sumo_repo_mysql:find_by(DocName, Conditions, Limit, Offset, State).

persist(Doc, State) ->
  sumo_repo_mysql:persist(Doc, State).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% fiar extensions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @doc Deletes the match through sumo_db and dispatch the event delete
fiar_delete(Match, DocName, State) ->
  MatchId = fiar_match:get_id(Match),
  {ok, _, _} = sumo_repo_mysql:delete(DocName, MatchId, State),
  sumo_event:dispatch(DocName, deleted, [Match]),
  {ok, {raw, Match}, State}.
