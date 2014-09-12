-module (fiar_handler).

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

rest_init(Req, State) ->
  {ok, Req, State}. 

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
  {true, Req, State}.

handle_get(Req, State) ->
  Matches = fiar_match_repo:get_matches(),
  MatchJson = fiar_match:match_list_to_json(Matches),
  RespBody = jiffy:encode(MatchJson),
  {RespBody, Req, State}.

handle_post(Req, State) ->
  {ok, Body, Req1} =  cowboy_req:body(Req),
  Decoded = jiffy:decode(Body, [return_maps]),

  Player1 = maps:get(<<"player1">>, Decoded),
  Player2 = maps:get(<<"player2">>, Decoded),

  Mid = fiar:start_match(Player1, Player2),
  Match = fiar_match_repo:get_match(Mid),
  MatchJson = fiar_match:to_json(Match),
  RespBody = jiffy:encode(MatchJson),
  Req2 = cowboy_req:set_resp_body(RespBody, Req1),

  {true, Req2, State}.