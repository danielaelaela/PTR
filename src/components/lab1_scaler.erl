-module(lab1_scaler).
-behaviour(gen_server).

-export([start_link/1, init/1, scale/1, handle_call/3, handle_cast/2, handle_info/2]).


start_link(Parent) ->
    gen_server:start_link(?MODULE, [Parent], []).

init([Parent]) ->
    io:format("~p: ~p~n", ["Scaler", self()]),
    erlang:start_timer(0, self(), timeout),

    {ok, {{0, 0, 0}, Parent}}.

scale(Pid) ->
    gen_server:cast(Pid, scale).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast(scale, State) ->
    {{Current, Previous, PPrevious}, Parent} = State,
    {noreply, {{Current + 1, Previous, PPrevious}, Parent}}.

handle_info({timeout, _, _}, State) ->
    {{Current, Previous, PPrevious}, Parent} = State,
    WorkerSup = lab1_pool_sup:get_child(Parent, lab1_worker_sup),

    PerSecond = Current - Previous,
    PrevPerSecond = Previous - PPrevious,
    RunningAvg = round(0.75 * PerSecond + 0.25 * PrevPerSecond),

    WorkerPids = supervisor:which_children(WorkerSup),
    TotalWorkers = length(WorkerPids),

    NrWorkers = RunningAvg div 100 - TotalWorkers + 3,
    % io:format("~p: ~p requests per second. Scaling ~p worker(s).~n", [self(), PerSecond, NrWorkers]),
    
    scale_pool(WorkerSup, NrWorkers),
    erlang:start_timer(1000, self(), timeout),
    {noreply, {{Current, Current, Previous}, Parent}}.


scale_pool(WorkerSup, NrWorkers) when NrWorkers > 0 ->
    lab1_worker_sup:start_worker(WorkerSup), 
    scale_pool(WorkerSup, NrWorkers - 1);

scale_pool(WorkerSup, NrWorkers) when NrWorkers < 0 ->
    [{_, WorkerPid, _, _} | _] = supervisor:which_children(WorkerSup),
    lab1_worker_sup:stop_worker(WorkerSup, WorkerPid),
    scale_pool(WorkerSup, NrWorkers + 1);

scale_pool(_, NrWorkers) when NrWorkers == 0 -> 
    ok.
