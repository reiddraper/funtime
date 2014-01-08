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
         result_millis/1,
         %%bench/3,
         bench/2]).

%% dialyzer
-export([micros_to_millis/1]).

%%%===================================================================
%%% Types
%%%===================================================================

-record(result, {
          raw_timings    :: timings(),
          minimum        :: timing(),
          maximum        :: timing(),
          mean           :: timing(), %% arithmetic mean
          std_dev        :: timing()
                }).
-type result()  :: #result{}.

%%-record(options, {
%%          min_runs :: non_neg_integer(),
%%          max_runs :: non_neg_integer(),
%%          min_time :: millis(),
%%          max_time :: millis()
%%         }).
%%-type options()  :: #options{}.

-type nullary() :: fun(() -> term()).
-type micros()  :: integer().
-type millis()  :: float() | integer().
-type timing()  :: micros() | millis().
-type timings() :: [timing()].

%%%===================================================================
%%% Public API
%%%===================================================================

-spec bench(nullary()) -> result().
bench(Fun) ->
    bench(Fun, 100).

-spec bench(nullary(), non_neg_integer()) -> result().
bench(Fun, NumTimes) ->
    RawTimes = run_for_at_least(timer:seconds(2), NumTimes, Fun),
    result_from_timings(RawTimes).


%%%===================================================================
%%% Helpers
%%%===================================================================

%% @doc Like `timer:tc/1', but throws away the result.
-spec tc_(nullary()) -> micros().
tc_(Fun) ->
    {Time, _Res} = timer:tc(Fun),
    Time.

-spec run_for_at_least(millis(), non_neg_integer(), nullary()) ->
    timings().
run_for_at_least(Milliseconds, Times, Function) ->
    %% First run `Times' times, and only
    %% then start checking if we've run
    %% for enough time:
    StartTime = os:timestamp(),
    CountTimes = [tc_(Function) || _ <- lists:seq(1, Times)],
    EndTime = os:timestamp(),
    TimeDiff = timer:now_diff(EndTime, StartTime),
    TimeDiffMilli = micros_to_millis(TimeDiff),
    case TimeDiffMilli >= Milliseconds of
        true ->
            CountTimes;
        false ->
            CountTimes ++
            run_until((Milliseconds - TimeDiffMilli) + TimeDiffMilli, Function)
    end.

run_until(Millis, Function) ->
    run_until(Millis, Function, []).

run_until(Millis, Function, Acc) ->
    case micros_to_millis(os:timestamp()) > Millis of
        true ->
            Acc;
        false ->
            run_until(Millis, Function, [tc_(Function) | Acc])
    end.


-spec micros_to_millis(timing()) -> millis().
micros_to_millis(Micros) ->
    Micros / 1000.

result_from_timings(Timings) ->
    #result{raw_timings = Timings,
            minimum     = lists:min(Timings),
            maximum     = lists:max(Timings),
            mean        = funtime_stats:mean(Timings),
            std_dev     = funtime_stats:std_dev(Timings)}.

map_results(Fun, #result{raw_timings   = RawTimings,
                         minimum       = Minimum,
                         maximum       = Maximum,
                         mean          = Mean,
                         std_dev       = StdDev}) ->
    #result{raw_timings = [Fun(X) || X <- RawTimings],
            minimum     = Fun(Minimum),
            maximum     = Fun(Maximum),
            mean        = Fun(Mean),
            std_dev     = Fun(StdDev)}.

-spec result_millis(result()) -> result().
result_millis(Results) ->
    map_results(fun micros_to_millis/1, Results).
