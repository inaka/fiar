-module (fiar_sup).
-author('euen@inakanetworks.com').
-behaviour(supervisor).

-export([init/1]).
-export([start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  KatanaRandom =
      {ktn_random,
       {ktn_random, start_link, []},
       permanent,
       5000,
       worker,
       [ktn_random]
      },
  {ok, {{one_for_one, 10, 60},
    [KatanaRandom]
    }
  }.