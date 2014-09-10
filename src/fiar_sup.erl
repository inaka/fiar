-module (fiar_sup).
-author('euen@inakanetworks.com').
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, start_match/2]).

start_link() ->
  Pid = supervisor:start_link({local, ?MODULE}, ?MODULE, []),
  lager:info("StartSupervisor"),
  Pid.

init([]) ->
  {ok, {{simple_one_for_one, 10, 60},
    [{fiar_match_repo,
      {fiar_match_repo, start, []},
      permanent, 5000, worker, [fiar_match_repo]}]
    }
  }.

start_match(Player1, Player2) ->
  supervisor:start_child(?MODULE, [Player1, Player2]).