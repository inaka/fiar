-module (fiar_handle).

-export(
 [
  init/3,
  rest_init/2,
 ]
).

%% cowboy
init(_Transport, _Req, _Opts) ->
    {upgrade, protocol, cowboy_rest}.

rest_init(Req, State) ->
  {ok, Req, State}.