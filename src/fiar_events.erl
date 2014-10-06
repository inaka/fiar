-module(fiar_events).
-author('euen@inakanetworks.com').

-export([start_link/0, notify/2]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Public API
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% @doc Spawns and links a new process and adds a handler to the
%%      fiar_events event manager
-spec start_link() -> {ok, pid()}.
start_link() -> 
  {ok, Pid} = gen_event:start_link({local, fiar_events}),
  gen_event:add_handler(fiar_events, fiar_event_handler, {}),
  {ok, Pid}.

notify(EventName, User) ->
  gen_event:notify(fiar_events, {EventName, User}).