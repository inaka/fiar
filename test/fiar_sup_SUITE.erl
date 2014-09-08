% This file is licensed to you under the Apache License,
% Version 2.0 (the "License"); you may not use this file
% except in compliance with the License.  You may obtain
% a copy of the License at
% 
% http://www.apache.org/licenses/LICENSE-2.0
%
% Unless required by applicable law or agreed to in writing,
% software distributed under the License is distributed on an
% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
% KIND, either express or implied.  See the License for the
% specific language governing permissions and limitations
% under the License.

%% @doc Third excercise: .
%%    == Required Knowledge ==
%%    <ul>
%%    <li>Modules and functions</li>
%%    <li>List functions</li>
%%    <li>Basic Erlang Structures</li>
%%    <li>Pattern Matching</li>
%%    <li>Gen Server</li>
%%    <li>Supervisor</li>
%%    </ul>
%%
%%    == Problem Statement ==
%%    Create fiar_sup, a simple_one_for_one supervisor
%%    that will monitor matches.
%%    <dl>
%%    <dt>start_link() -> {ok, pid()}.</dt>
%%    <dt>start_match() -> {ok, pid()}.</dt>
%%    </dl>

-module(fiar_sup_SUITE).
-author('euen@inakanetworks.com').

-type config() :: [{atom(), term()}].
-export([all/0]).
-export([start/1]).

%% @private
-spec all() -> [atom()].
all() -> [Fun || {Fun, 1} <- module_info(exports), Fun =/= module_info].

-spec start(config()) -> {ok, pid()}.
start(_Config) ->
  fiar_sup:start_link().