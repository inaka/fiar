-module (fiar_users_handler).

-export(
 [
  init/3,
  rest_init/2,
  allowed_methods/2,
  content_types_accepted/2,
  is_authorized/2,
  handle_post/2
 ]
).

%% cowboy
init(_Transport, _Req, _Opts) ->
  {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) ->
  {ok, Req, State}. 

allowed_methods(Req, State) ->
  {[<<"POST">>, <<"OPTIONS">>], Req, State}.

content_types_accepted(Req, State) ->
  {[
    {<<"application/json">>, handle_post}
   ],
   Req, State}.

is_authorized(Req, State) ->
  {true, Req, State}.

handle_post(Req, State) ->
  {ok, Body, Req1} =  cowboy_req:body(Req),
  try
    Decoded = jiffy:decode(Body, [return_maps]),
    Username = maps:get(<<"username">>, Decoded),
    User = fiar:new_user(Username),
    UserJson = fiar_user:to_json(User, private),
    RespBody = jiffy:encode(UserJson),
    Req2 = cowboy_req:set_resp_body(RespBody, Req1),
    {true, Req2, State}
  catch
    _:Exception ->
      lager:warning("Exception in POST: ~p~n", [Exception]),
      fiar_utils:handle_exception(Exception, Req1, State)
  end.
