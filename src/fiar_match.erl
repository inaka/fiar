-module(fiar_match).
-export([start/0, play/2, stop/1]).

%-type col() :: 1..7.

-behaviour(gen_server).
    
% gen_server callbacks
-export([init/1,
         handle_call/3, 
         handle_cast/2,
         handle_info/2, 
         terminate/2, 
         code_change/3]).


%%====================================================================
%% Server interface
%%====================================================================
%% Booting server (and linking to it)
start() -> 
    gen_server:start({local, ?MODULE}, ?MODULE, [], []).

%% Stopping server asynchronously
stop(Pid) ->
    gen_server:cast(Pid, shutdown).

%-spec play(pid(), col()) -> won | drawn | next.
play(Pid, Col) -> 
  case gen_server:call(Pid, {play, Col}) of
    {ok, Reply} -> Reply;
    {error, Ex} -> throw(Ex)
  end.

%%====================================================================
%% gen_server callbacks
%%====================================================================
init([]) ->
    %process_flag(trap_exit, true),
    EmptyState = fiar_core:start(),
    {ok, EmptyState}.

%% Synchronous, possible return values  
% {reply,Reply,NewState} 
% {reply,Reply,NewState,Timeout}
% {reply,Reply,NewState,hibernate}
% {noreply,NewState}
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,Reply,NewState} 
% {stop,Reason,NewState}
handle_call({play, Col}, _From, State) ->
 try
    {Reply, NewState} = case fiar_core:play(Col, State) of
      {Result, NextState} -> {Result, NextState};
      Result -> {Result, State}
    end,
    {reply, {ok, Reply}, NewState}
  catch
    _:Ex ->
        {reply, {error, Ex}, State}
  end.

%% Asynchronous, possible return values
% {noreply,NewState} 
% {noreply,NewState,Timeout}
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
%% normal termination clause
handle_cast(shutdown, State) ->
    io:format("Generic cast handler: *shutdown* while in '~p'~n",[State]),
    {stop, normal, State};
%% generic async handler
handle_cast(Message, State) ->
    io:format("Generic cast handler: '~p' while in '~p'~n",[Message, State]),
    {noreply, State}.

%% Informative calls
% {noreply,NewState} 
% {noreply,NewState,Timeout} 
% {noreply,NewState,hibernate}
% {stop,Reason,NewState}
handle_info(Message, State) -> 
    io:format("Generic info handler: '~p' '~p'~n",[Message, State]),
    {noreply, State}.

%% Server termination
terminate(_Reason, _Server) -> 
    io:format("Generic termination handler: '~p' '~p'~n",[_Reason, _Server]).


%% Code change
code_change(_OldVersion, _Server, _Extra) -> {ok, _Server}.
  