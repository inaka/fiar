-module (fiar_utils).

-export([
         handle_exception/3
        ]).

-type datetime() ::
        {
          datetime,
          {
            {integer(), integer(), integer()},
            {integer(), integer(), integer()}
          }
        }.
-export_type([datetime/0]).

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
  {halt, cowboy_req:req(), term()}.
handle_exception(invalid_player, Req, State) ->
  {ok, Req1} = cowboy_req:reply(403, Req),
  {halt, Req1, State};
handle_exception(bad_request, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, Req),
  {halt, Req1, State};
handle_exception(invalid_column, Req, State) ->
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
  ST = erlang:get_stacktrace(),
  lager:error("~p - ~p", [Reason, ST]),
  {ok, Req1} = cowboy_req:reply(500, Req),
  {halt, Req1, State}.