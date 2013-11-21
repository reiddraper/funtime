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

-module(funtime).

-ifdef(TEST).
-compile(export_all).
-endif.

-export([bench/1,
         bench/2,
         result_millis/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(result, {raw_timings    :: [micros()],
                 minimum        :: micros(),
                 maximum        :: micros(),
                 mean           :: float() %% arithmetic mean
                }).

-type result()  :: #result{}.
-type nullary() :: fun(() -> term()).
-type micros()  :: integer().
-type timings() :: [micros()].

%%%===================================================================
%%% Public API
%%%===================================================================

-spec bench(nullary()) -> result().
bench(Fun) ->
    bench(Fun, 100).

-spec bench(nullary(), non_neg_integer()) -> result().
bench(Fun, NumTimes) ->
    RawTimes = [tc_(Fun) || _ <- lists:seq(1, NumTimes)],
    #result{raw_timings = RawTimes,
            minimum     = lists:min(RawTimes),
            maximum     = lists:max(RawTimes),
            mean        = mean(RawTimes)}.

-spec result_millis(result()) -> result().
result_millis(#result{raw_timings   = RawTimings,
                      minimum       = Minimum,
                      maximum       = Maximum,
                      mean          = Mean}) ->
    #result{raw_timings = [micros_to_millis(X) || X <- RawTimings],
            minimum     = micros_to_millis(Minimum),
            maximum     = micros_to_millis(Maximum),
            mean        = micros_to_millis(Mean)}.

%%%===================================================================
%%% Helpers
%%%===================================================================

%% @doc Like `timer:tc/1', but throws away the result.
-spec tc_(nullary()) -> micros().
tc_(Fun) ->
    {Time, _Res} = timer:tc(Fun),
    Time.

-spec mean(timings()) -> float().
mean(Timings) ->
    lists:sum(Timings) / length(Timings).

-spec micros_to_millis(micros()) -> float().
micros_to_millis(Micros) ->
    Micros / 1000.
