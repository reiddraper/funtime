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

-module(funtime_stats).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([mean/1,
         std_dev/1]).

%%%===================================================================
%%% Types
%%%===================================================================

%%%===================================================================
%%% Public API
%%%===================================================================

-spec mean(funtime:timings()) -> funtime:millis().
mean(Timings) ->
    lists:sum(Timings) / length(Timings).

std_dev(Timings) ->
    Mean = mean(Timings),
    DiffSquared = [math:pow(X - Mean, 2) || X <- Timings],
    Variance = mean(DiffSquared),
    math:sqrt(Variance).


%%%===================================================================
%%% Helpers
%%%===================================================================
