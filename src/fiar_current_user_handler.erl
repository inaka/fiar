-module (fiar_current_user_handler).

-export(
 [ init/3
 , rest_init/2
 , allowed_methods/2
 , content_types_provided/2
 , is_authorized/2
 , handle_get/2
 ]
).

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) ->
  {ok, Req, State}. 

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"OPTIONS">>], Req, State}.

content_types_provided(Req, State) ->
  {[
    {{<<"application">>, <<"json">>, []}, handle_get}
   ],
   Req,
   State}.

is_authorized(Req, State) ->
  case fiar_auth:check_auth(Req) of
    {authenticated, User, _Req1} ->
      {true, Req, #{user => User}};
    {not_authenticated, AuthHeader, Req1} ->
      {{false, AuthHeader}, Req1, State}
  end.

handle_get(Req, State) ->
  try
    User = maps:get(user, State),
    User1 = fiar_user:to_json(User, public),
    CurrentMatches = fiar:current_matches(User),
    CurrentMatchesJson = fiar_match:matches_to_json(CurrentMatches),
    Response = [{user, User1}, {current_matches, CurrentMatchesJson}],
    RespBody = jiffy:encode({Response}),
    {RespBody, Req, State}
  catch
    _:Exception ->
      lager:warning("Exception in GET: ~p~n", [Exception]),
      fiar_utils:handle_exception(Exception, Req, State)
  end.