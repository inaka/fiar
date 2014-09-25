-module (fiar_notify_handler).
-behavior(lasse_handler).

-export([
         init/3,
         handle_notify/2,
         handle_info/2,
         handle_error/3,
         terminate/3
        ]).

init(_InitArgs, _LastEventId, Req) ->
  case fiar_auth:check_auth(Req) of
    {authenticated, User, Req1} ->
      {MatchId, Req2} =  cowboy_req:binding(match_id, Req1),
      process = list_to_atom(MatchId ++ "_" ++ Username),
      erlang:register(process, self()),
      {ok, Req2, #{user => User}};
    {not_authenticated, _AuthHeader, Req1} ->
        {shutdown, 403, [], [], Req1, #{}}
  end.

handle_notify(ping, State) ->
    {send, [{data, <<"">>}], State}.

handle_info(_Msg, State) ->
    {nosend, State}.

handle_error(_Msg, _Reason, State) ->
    State.

terminate(_Reason, _Req, _State) ->
    ok.
