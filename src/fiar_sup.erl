-module (fiar_sup).
-author('euen@inakanetworks.net').
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0, start_match/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{simple_one_for_one, 10, 60},
    [{fiar_match, {fiar_match, start, []},
    permanent, 5000, worker, [fiar_match]}]
    }
  }.

start_match() ->
  supervisor:start_child(?MODULE, []).