-module (fiar_sup).
-author('euen@inakanetworks.com').
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 10, 60},
    []
    }
  }.