-module (fiar_user_repo).

-export([start/1, get_user/1, get_users/0]).

start(Username) ->
  User = fiar_user:new(Username),
  lager:info("User previous to save: ~p", [User]),
  StoredUser = sumo:persist(fiar_user, User),
  fiar_user:get_id(StoredUser).

get_user(Uid) ->
  case sumo:find(fiar_user, Uid) of
    notfound -> throw({notfound, Uid});
    U -> U
  end.

get_users() ->
  sumo:find_all(fiar_user).