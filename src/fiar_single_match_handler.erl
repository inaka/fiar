-module (fiar_single_match_handler).

-export(
 [
  init/3,
  rest_init/2,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  is_authorized/2,
  handle_get/2,
  handle_put/2
 ]
).

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) ->
  {ok, Req, State}. 

allowed_methods(Req, State) ->
  {[<<"GET">>, <<"PUT">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, handle_put}
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
  {MatchId, Req1} =  cowboy_req:binding(match_id, Req),
  try
    Match = fiar:get_match(MatchId, maps:get(user, State)),
    MatchJson = fiar_match:to_json(Match),
    RespBody = jiffy:encode(MatchJson),
    {RespBody, Req1, State}
  catch
    _:Exception ->
      lager:warning("Exception in GET: ~p~n", [Exception]),
      fiar_utils:handle_exception(Exception, Req1, State)
  end.

handle_put(Req, State) ->
  {MatchIdBin, Req1} = cowboy_req:binding(match_id, Req),
  try 
    MatchId = binary_to_integer(MatchIdBin, 10),
    {ok, Body, Req2} =  cowboy_req:body(Req1),
    Col = maps:get(<<"column">>, jiffy:decode(Body, [return_maps])),
    User = maps:get(user, State),
    fiar:play(MatchId, Col, User),
    Match = fiar:get_match(MatchId, User),
    MatchJson = fiar_match:to_json(Match),
    RespBody = jiffy:encode(MatchJson),
    Req3 = cowboy_req:set_resp_body(RespBody, Req2),
    {true, Req3, State}
  catch
    _:Exception ->
      lager:warning("Exception in PUT: ~p~n", [Exception]),
      fiar_utils:handle_exception(Exception, Req1, State)
  end.
