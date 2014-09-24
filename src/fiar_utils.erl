-module (fiar_utils).

-export([ handle_exception/3
        , datetime_to_json/1
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

%% @doc Converts a datetime record into a binary representation of its data.
-spec datetime_to_json(datetime()) -> binary().
datetime_to_json({{Yi, Mi, Di}, {Hi, Ni, Si}}) ->
  Y = integer_to_list(Yi),
  M = integer_to_list(Mi),
  D = integer_to_list(Di),
  H = integer_to_list(Hi),
  N = integer_to_list(Ni),
  S = integer_to_list(Si),
  iolist_to_binary([Y, "-", M, "-", D, "T", H, ":", N, ":", S, ".000000Z"]).

-spec handle_exception(atom(), cowboy_req:req(), term()) ->
  {halt, cowboy_req:req(), term()}.
handle_exception(match_finished, Req, State) ->
  {ok, Req1} = cowboy_req:reply(400, Req),
  {halt, Req1, State};
handle_exception(invalid_player, Req, State) ->
  {ok, Req1} = cowboy_req:reply(403, Req),
  {halt, Req1, State};
handle_exception(rival_notfound, Req, State) ->
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
handle_exception(notfound, Req, State) ->
  {ok, Req1} = cowboy_req:reply(404, Req),
  {halt, Req1, State};
handle_exception(conflict, Req, State)->
  {ok, Req1} = cowboy_req:reply(409, Req),
  {halt, Req1, State}.
