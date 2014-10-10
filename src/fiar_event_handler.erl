%%% @doc Fiar Generic Event Handler.
%%% @end
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-module(fiar_event_handler).
-author('euen@inakanetworks.com').

-behaviour(gen_event).

-export([start_link/0]).
-export([init/1, handle_event/2, handle_call/2,
         handle_info/2, terminate/2, code_change/3]).

-record(state, {}).
-type state() :: #state{}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Spawns and links a new process and adds a handler to the
%%      fiar_events event manager
-spec start_link() -> {ok, pid()}.
start_link() -> 
  {ok, _} = fiar_events:start_link().

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init({}) -> {ok, state()}.
init({}) ->
  {ok, #state{}}.

-spec handle_event({module(), atom(), term()}, state()) -> {ok, state()}.
handle_event({fiar_match, updated, [Match]}, State) ->
  try
    match_ended_notify(Match),
    MatchId = fiar_match:get_id(Match),
    UserId = fiar_match:get_player(Match),
    fiar:notify(match_updated, MatchId, UserId, Match)
  catch
    _:Exception ->
      lager:warning(
        "Could not deliver notification: ~p~nStack: ~p",
        [Exception, erlang:get_stacktrace()])
  end,
  {ok, State};
handle_event({fiar_match, created, [Match]}, State) ->
  try
    fiar:broadcast(match_started, Match)
  catch
    _:Exception ->
      lager:warning(
        "Could not deliver notification: ~p~nStack: ~p",
        [Exception, erlang:get_stacktrace()])
  end,
  {ok, State};
handle_event({fiar_match, deleted, [Match]}, State) ->
  try
    MatchId = fiar_match:get_id(Match),
    Player1 = fiar_match:get_player1(Match),
    Player2 = fiar_match:get_player2(Match),
    fiar:notify(match_deleted, MatchId, Player1, Match),
    fiar:notify(match_deleted, MatchId, Player2, Match),
    fiar:broadcast(match_ended, Match)
  catch
    _:Exception ->
      lager:warning(
        "Could not deliver notification: ~p~nStack: ~p",
        [Exception, erlang:get_stacktrace()])
  end,
  {ok, State};
handle_event({fiar_user, connected, [User]}, State) ->
  try
    fiar:broadcast(user_conected, User)
  catch
    _:Exception ->
      lager:warning(
        "Could not deliver notification: ~p~nStack: ~p",
        [Exception, erlang:get_stacktrace()])
  end,
  {ok, State};
handle_event({fiar_user, disconnected, [User]}, State) ->
  try
    fiar:broadcast(user_disconnected, User)
  catch
    _:Exception ->
      lager:warning(
        "Could not deliver notification: ~p~nStack: ~p",
        [Exception, erlang:get_stacktrace()])
  end,
  {ok, State};
handle_event(Event, State) ->
  lager:info("Unhandled event: ~p~n", [Event]),
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unused Callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-spec handle_call(Req, state()) ->
  {ok, {unexpected_call, Req}, state()}.
handle_call(Req, State) ->
  {ok, {unexpected_call, Req}, State}.

-spec handle_info(term(), state()) ->
  {ok, state()}.
handle_info(_Info, State) ->
  {ok, State}.

-spec terminate(term(), state()) ->
  ok.
terminate(_Reason, _State) ->
  ok.

-spec code_change(term(), state(), term()) ->
  {ok, state()}.
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Private Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

match_ended_notify(Match) ->
  case fiar_match:get_status(Match) of
    on_course -> ok;
    _ -> fiar:broadcast(match_ended, Match)
  end.