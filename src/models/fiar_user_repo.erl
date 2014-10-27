-module (fiar_user_repo).

-export([ create/2
        , get/2
        , find_by_username/1
        , find_by_id/1
        ]).

-type user() ::
        #{
           id => integer(),
           username => string(),
           pass => string(),
           created_at => fiar_utils:datetime(),
           updated_at => fiar_utils:datetime()
         }.

-export_type([user/0]).

create(Username, Pass) ->
  case find_by_username(Username) of
    notfound -> 
      NewUser = fiar_user:new(Username, Pass),
      sumo:persist(fiar_user, NewUser);
    _User ->
      throw(conflict)
  end.

-spec get(string(), string()) -> notfound | user().
get(Username, Pass) ->
  case sumo:find_by(fiar_user, [{username, Username}, {pass, Pass}]) of
    []     -> notfound;
    [User] -> User
  end.

-spec find_by_username(binary()) -> notfound | user().
find_by_username(Username) ->
  case sumo:find_by(fiar_user, [{username, Username}]) of
    [User] -> User;
    _      -> notfound
  end.

-spec find_by_id(integer()) -> notfound | user().
find_by_id(UserId) ->
  case sumo:find(fiar_user, UserId) of
    notfound -> throw(notfound);
    User -> User
  end.