%% -------------------------------------------------------------------
%% Copyright (c) 2013 Reid Draper. All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

%% @doc

-module(funtime_sample).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([factorial/1,
         fibonacci/1]).

%%%===================================================================
%%% Public API
%%%===================================================================

-spec factorial(integer()) -> integer().
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

-spec fibonacci(integer()) -> integer().
fibonacci(0) -> 0;
fibonacci(1) -> 1;
fibonacci(N) -> fibonacci(N - 1) + fibonacci(N - 2).
