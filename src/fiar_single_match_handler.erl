-module (fiar_single_match_handler).

-export(
 [
  init/3,
  rest_init/2,
  allowed_methods/2,
  content_types_accepted/2,
  content_types_provided/2,
  is_authorized/2,
  handle_get/2
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
    {<<"application/json">>, handle_post}
   ],
   Req, State}.

content_types_provided(Req, State) ->
  {[{{<<"application">>, <<"json">>, []}, handle_get}], Req, State}.

is_authorized(Req, State) ->
  {true, Req, State}.

handle_get(Req, State) ->
  {MatchId, Req1} =  cowboy_req:binding(match_id, Req),
  Match = fiar_match_repo:get_match(MatchId),
  MatchJson = fiar_match:to_json(Match),
  RespBody = jiffy:encode(MatchJson),
  {RespBody, Req1, State}.