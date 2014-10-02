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
  {ok, Pid} = gen_event:start_link(fiar_events),
  gen_event:add_handler(fiar_events, fiar_event_handler, {}),
  {ok, Pid}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Callback implementation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-spec init({}) -> {ok, state()}.
init({}) ->
  {ok, #state{}}.

-spec handle_event({module(), atom(), term()}, state()) -> {ok, state()}.
handle_event({fiar_match, created, [Match]}, State) ->
  MatchId = fiar_match:get_id(Match),
  UserId = fiar_match:get_player(Match),
  Process = fiar_utils:process_name(MatchId, UserId),
  lasse_handler:notify(Process, Match),
  {ok, State};
handle_event(_Event, State) ->
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