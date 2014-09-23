-module (fiar_user_repo).

-export([ create/1
        , get/2
        , find_by_username/1
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

create(Username) ->
  case find_by_username(Username) of
    notfound -> 
          NewUser = fiar_user:new(Username),
          lager:info("User previous to save: ~p", [NewUser]),
          StoredUser = sumo:persist(fiar_user, NewUser);
    User ->
          throw(conflict)
  end.

-spec get(string(), string()) -> not_found | user().
get(Username, Pass) ->
  case sumo:find_by(fiar_user, [{username, Username}, {pass, Pass}]) of
    []     -> not_found;
    [User] -> User
  end.

find_by_username(Username) ->
  case sumo:find_by(fiar_user, [{username, Username}]) of
    [User] -> User;
    _      -> notfound
  end.
