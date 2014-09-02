-module(fiar_match).
-author('euen@inakanetworks.com').
-behaviour(gen_server).

-type from() :: {pid(), _}.
-type reason() :: normal | shutdown | {shutdown, term()} | term().
-type oldVsn() :: term() | {down, term()}.
-record(state, {match::fiar_core:match()}).
-type state() :: #state{}.

-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).
-export([start/0, play/2, stop/1]).

-spec start() -> {ok, pid()}.
start() ->
    gen_server:start_link(?MODULE, [], []).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, shutdown).

-spec play(pid(), fiar_core:col()) -> won | drawn | next.
play(Pid, Col) -> 
  case gen_server:call(Pid, {play, Col}) of
    {ok, Reply} -> Reply;
    {error, Ex} -> throw(Ex)
  end.

-spec init([]) -> {ok, state()}.
init([]) ->
    EmptyMatch = fiar_core:start(),
    {ok, #state{match = EmptyMatch}}.

-spec handle_call({play, fiar_core:col()}, from(), state()) ->
                  {reply, {error, any()}, state()} | 
                  {reply, {ok, won | drawn | next}, state()}.
handle_call({play, Col}, _From, State = #state{match = Match}) ->
  try
    {Reply, NewMatch} =
      case fiar_core:play(Col, Match) of
        {Result, NextMatch} -> {Result, NextMatch};
        Result -> {Result, Match}
      end,
    {reply, {ok, Reply}, State#state{match = NewMatch}}
  catch
    _:Ex -> io:format("~p~n", [erlang:get_stacktrace()]),
        {reply, {error, Ex}, State}
  end.

-spec handle_cast(shutdown | string(), state()) ->
  {stop, normal, state()} | {noreply, state()}.
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

-spec handle_info(string(), state()) -> {noreply, state()}.
handle_info(Message, State) -> 
    io:format("Generic info handler: '~p' '~p'~n",[Message, State]),
    {noreply, State}.

-spec terminate(reason(), state()) -> ok.
terminate(_Reason, _State) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _State]).

-spec code_change(oldVsn(), state(), term()) ->
                  {ok | error, state() | reason()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
  