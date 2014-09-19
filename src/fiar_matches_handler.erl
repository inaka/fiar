-module (fiar_matches_handler).

-export(
 [
  init/3,
  rest_init/2,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  is_authorized/2,
  handle_get/2,
  handle_post/2
 ]
).

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, _Opts) ->
  {ok, Req, #{}}. 

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, handle_post}
   ],
   Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, handle_get}], Req, State}.

is_authorized(Req, State) ->
    case fiar_auth:check_auth(Req) of
        {authenticated, User, _Req1} ->
            {true, Req, #{user => User}};
        {not_authenticated, AuthHeader, Req1} ->
            {{false, AuthHeader}, Req1, State}
    end.

handle_get(Req, State) ->
  Matches = fiar:get_matches(maps:get(user, State)),
  lager:info("matchess : ~p~n", [Matches]),
  MatchJson = fiar_match:match_list_to_json(Matches),
  RespBody = jiffy:encode(MatchJson),
  {RespBody, Req, State}.

handle_post(Req, State) ->
  {ok, Body, Req1} =  cowboy_req:body(Req),
  try
    Decoded = jiffy:decode(Body, [return_maps]),
    Username2 = maps:get(<<"player2">>, Decoded),
    User1 = maps:get(user, State),
    User2 = 
      case fiar:find_by_username(Username2) of
        []     -> throw(bad_request);
        [User] -> User;
        _      -> throw({multiple_users, Username2})
      end,
    Mid = fiar:start_match(User1, User2),
    Match = fiar:get_match(Mid),
    MatchJson = fiar_match:to_json(Match),
    RespBody = jiffy:encode(MatchJson),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
    {true, Req2, State}
  catch
    _:Exception ->
      lager:info("Exception in POST: ~p~n", [Exception]),
      fiar_utils:handle_exception(Exception, Req1, State)
  end.