-module(fiar_match).
-author('euen@inakanetworks.net').
-export([start/0, play/2, stop/1]).

-behaviour(gen_server).
    
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).

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

-spec init([]) -> {ok, fiar:state()}.
init([]) ->
    EmptyState = fiar_core:start(),
    {ok, EmptyState}.

-spec handle_call({play, fiar_core:col()}, fiar:from(), fiar:state()) ->
                  {reply, {error, any()}, fiar:state()} | 
                  {reply, {ok, won | drawn | next}, fiar:state()}.
handle_call({play, Col}, _From, State) ->
  try
    {Reply, NewState} =
      case fiar_core:play(Col, State) of
        {Result, NextState} -> {Result, NextState};
        Result -> {Result, State}
      end,
    {reply, {ok, Reply}, NewState}
  catch
    _:Ex ->
        {reply, {error, Ex}, State}
  end.

-spec handle_cast(shutdown | string(), fiar:state()) ->
  {stop, normal, fiar:state()} | {noreply, fiar:state()}.
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

-spec handle_info(string(), fiar:state()) -> {noreply, fiar:state()}.
handle_info(Message, State) -> 
    io:format("Generic info handler: '~p' '~p'~n",[Message, State]),
    {noreply, State}.

-spec terminate(fiar:reason(), fiar:state()) -> ok.
terminate(_Reason, _State) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _State]).

-spec code_change(fiar:oldVsn(), fiar:state(), term()) ->
                  {ok | error, fiar:state() | fiar:reason()}.
code_change(_OldVersion, State, _Extra) -> {ok, State}.
  