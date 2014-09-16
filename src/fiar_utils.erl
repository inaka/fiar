-module (fiar_utils).

-export([
         handle_exception/3
        ]).

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
  {halt, cowboy_req:req(), term()}.
handle_exception(bad_request, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, Req),
  {halt, Req1, State};
handle_exception(bad_key, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, Req),
  {halt, Req1, State};
handle_exception(badarg, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, Req),
  {halt, Req1, State};
handle_exception(not_found, Req, State) ->
  {ok, Req1} = cowboy_req:reply(404, Req),
  {halt, Req1, State};
handle_exception(too_large, Req, State)->
  {ok, Req1} = cowboy_req:reply(413, Req),
  {halt, Req1, State};
handle_exception(conflict, Req, State)->
  {ok, Req1} = cowboy_req:reply(409, Req),
  {halt, Req1, State};
handle_exception(Reason, Req, State) ->
  lager:error("~p", [Reason]),
  {ok, Req1} = cowboy_req:reply(500, Req),
  {halt, Req1, State}.