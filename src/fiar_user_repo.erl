-module (fiar_user_repo).

-export([ create/1
        , get_user/1
        , get_users/0
        ]).

create(Username) ->
  case find_by_username(Username) of
    [User|_] ->
          throw(conflict);
    [] -> 
          NewUser = fiar_user:new(Username),
          lager:info("User previous to save: ~p", [NewUser]),
          StoredUser = sumo:persist(fiar_user, NewUser),
          fiar_user:get_id(StoredUser)
  end.

get_user(Uid) ->
  case sumo:find(fiar_user, Uid) of
    notfound -> throw({notfound, Uid});
    U -> U
  end.

get_users() ->
  sumo:find_all(fiar_user).

find_by_username(Username) ->
  sumo:find_by(fiar_user, [{username, Username}]).