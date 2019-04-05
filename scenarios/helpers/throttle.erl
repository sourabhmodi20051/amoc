%%%-------------------------------------------------------------------
%%% @author denys
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Mar 2019 14:24
%%%-------------------------------------------------------------------
-module(throttle).
-author("denys").


%% API
-export([start/2,
         start/3,
         start/4,
         send/3,
         send/2,
         send_and_wait/2,
         run/2,
         stop/1]).

-define(DEFAULT_INTERVAL, 60000).%% one minute
-define(DEFAULT_MSG_TIMEOUT, 60000).%% one minute

-define(RATE(Name), [throttle, Name, rate]).
-define(EXEC_RATE(Name), [throttle, Name, exec_rate]).
-define(REQ_RATE(Name), [throttle, Name, req_rate]).



-record(state, {can_run_fn = true :: boolean(),%%ms
                n :: non_neg_integer(),
                interval = ?DEFAULT_INTERVAL :: non_neg_integer(),
                delay_between_executions = 0 :: non_neg_integer(),
                schedule = [] :: [fun(()-> any())],
                schedule_reversed = [] :: [fun(()-> any())]}).

-type name() :: atom().

-spec start(name(), pos_integer()) -> ok|{error, any()}.
start(Name, Rate) ->
    start(Name, Rate, ?DEFAULT_INTERVAL).

-spec start(name(), pos_integer(), non_neg_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval) ->
    start(Name, Rate, Interval, 10).

-spec start(name(), pos_integer(), non_neg_integer(), pos_integer()) -> ok | {error, any()}.
start(Name, Rate, Interval, NoOfProcesses) ->
    case pg2:get_members(Name) of
        {error, {no_such_group, Name}} ->
            pg2:create(Name),
            amoc_metrics:init(gauge, ?RATE(Name)),
            [amoc_metrics:init(counters, E) || E <- [?EXEC_RATE(Name), ?REQ_RATE(Name)]],
            RatePerMinute = rate_per_minute(Rate, Interval),
            amoc_metrics:update_gauge(?RATE(Name), RatePerMinute),
            RealNoOfProcesses = min(Rate, NoOfProcesses),
            start_throttle_processes(Name, Interval, Rate, RealNoOfProcesses);
        List when is_list(List) ->
            {error, {name_is_already_used, Name}}
    end.

-spec run(name(), fun(()-> any())) -> ok | {error, any()}.
run(Name, Fn) ->
    case get_throttle_process(Name) of
        {ok, Pid} ->
            amoc_metrics:update_counter(?REQ_RATE(Name)),
            Fun =
                fun() ->
                    amoc_metrics:update_counter(?EXEC_RATE(Name)),
                    Fn()
                end,
            Pid ! {run, Fun};
        Error -> Error
    end.

-spec send(name(), pid(), any()) -> ok | {error, any()}.
send(Name, Pid, Msg) ->
    run(Name, fun() -> Pid ! Msg end).

-spec send(name(), any()) -> ok | {error, any()}.
send(Name, Msg) ->
    send(Name, self(), Msg).

-spec send_and_wait(name(), any()) -> ok | {error, any()}.
send_and_wait(Name, Msg) ->
    send(Name, Msg),
    receive
        Msg -> ok
    end.

-spec stop(name()) -> ok|{error, any()}.
stop(Name) ->
    case pg2:get_members(Name) of
        List when is_list(List) ->
            pg2:delete(Name),
            [P ! stop_process || P <- List], ok;
        Error -> Error
    end.

rate_per_minute(_, 0) -> 0;
rate_per_minute(Rate, Interval) ->
    (Rate * 60000) div Interval.

start_throttle_processes(Name, Interval, Rate, 1) ->
    start_throttle_process(Name, Interval, Rate);
start_throttle_processes(Name, Interval, Rate, N) when is_integer(N), N > 1 ->
    ProcessRate = Rate div N,
    start_throttle_process(Name, Interval, ProcessRate),
    start_throttle_processes(Name, Interval, Rate - ProcessRate, N - 1).


start_throttle_process(Name, Interval, Rate) ->
    Pid = spawn(fun() -> init_throttle_process(Interval, Rate) end),
    pg2:join(Name, Pid).

get_throttle_process(Name) ->
    case pg2:get_members(Name) of
        [] ->
            {error, {no_trottle_process_registered, Name}};
        {error, Error} ->
            {error, Error};
        List -> %% nonempty list
            N = rand:uniform(length(List)),
            {ok, lists:nth(N, List)}
    end.

init_throttle_process(Interval, Rate) ->
    if
        Rate < 5 -> lager:error("too low rate, please reduce NoOfProcesses");
        true -> ok
    end,
    InitialState = case {Interval, Interval div Rate} of
                       {0, 0} -> %% limit only No of simultaneous executions
                           #state{interval                 = Interval,
                                  delay_between_executions = 0,
                                  n                        = Rate};
                       {_, I} when I < 10 ->
                           lager:error("too high rate, please increase NoOfProcesses"),
                           #state{interval                 = Interval,
                                  delay_between_executions = 10,
                                  n                        = Rate};
                       {_, DelayBetweenExecutions} ->
                           #state{interval                 = Interval,
                                  delay_between_executions = DelayBetweenExecutions,
                                  n                        = Rate}
                   end,
    throttle_process(InitialState).


throttle_process(State) ->
    NewState = maybe_run_fn(State),
    wait_for_msg(NewState).

maybe_run_fn(#state{schedule = [], schedule_reversed = []} = State) ->
    State;
maybe_run_fn(#state{schedule = [], schedule_reversed = SchRev} = State) ->
    NewSchedule = lists:reverse(SchRev),
    NewState = State#state{schedule = NewSchedule, schedule_reversed = []},
    maybe_run_fn(NewState);
maybe_run_fn(#state{can_run_fn = true, n = N} = State) when N > 0 ->
    NewState = run_fn(State),
    NewState#state{can_run_fn = false, n = N - 1};
maybe_run_fn(State) ->
    State.

run_fn(#state{schedule = [Fn | T], delay_between_executions = Delay, interval = Interval} = State) ->
    Self = self(),
    spawn(fun() -> run_and_wait(Fn, Interval), Self ! increase_n end),
    erlang:send_after(Delay, self(), delay_between_executions),
    State#state{schedule = T}.

run_and_wait(Fn, Interval) ->
    {TimeUs, _} = timer:tc(Fn),
    Time = erlang:convert_time_unit(TimeUs, microsecond, millisecond),
    case Interval - Time of
        SleepTime when SleepTime > 0 -> timer:sleep(SleepTime);
        _ -> ok
    end.

wait_for_msg(#state{n = N, schedule_reversed = SchRev} = State) ->
    receive
        stop_process -> ok;
        {run, Fn} ->
            throttle_process(State#state{schedule_reversed = [Fn | SchRev]});
        increase_n ->
            throttle_process(State#state{n = N + 1});
        delay_between_executions ->
            throttle_process(State#state{can_run_fn = true})
    after State#state.interval + ?DEFAULT_MSG_TIMEOUT ->
        lager:debug("throttle process is inactive (n=~p)", [State#state.n]),
        throttle_process(State)
    end.

