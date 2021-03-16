-module(lab1_scaler).
-behaviour(gen_server).

-export([start_link/0, init/1, routed/0, handle_cast/2, handle_info/2]).

-define(WORKER_SUP, lab1_worker_sup).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Scaler", self()]),
    erlang:start_timer(0, self(), timeout),
    {ok, {0, 0, 0}}.

routed() ->
    gen_server:cast(?MODULE, routed).


handle_cast(routed, State) ->
    {Current, Previous, PPrevious} = State,
    {noreply, {Current + 1, Previous, PPrevious}}.

handle_info({timeout, _, _}, State) ->
    {Current, Previous, PPrevious} = State,

    PerSecond = Current - Previous,
    PrevPerSecond = Previous - PPrevious,
    RunningAvg = round(0.75 * PerSecond + 0.25 * PrevPerSecond),

    WorkerPids = supervisor:which_children(?WORKER_SUP),
    TotalWorkers = length(WorkerPids),

    NrWorkers = RunningAvg div 100 - TotalWorkers + 3,
    io:format("~p: ~p requests per second. Scaling ~p worker(s).~n", ["Scaler", PerSecond, NrWorkers]),
    
    scale_pool(NrWorkers),
    erlang:start_timer(1000, self(), timeout),
    {noreply, {Current, Current, Previous}}.


scale_pool(NrWorkers) when NrWorkers > 0 ->
    ?WORKER_SUP:start_worker(), 
    scale_pool(NrWorkers - 1);

scale_pool(NrWorkers) when NrWorkers < 0 ->
    [{_, WorkerPid, _, _} | _] = supervisor:which_children(?WORKER_SUP),
    ?WORKER_SUP:stop_worker(WorkerPid),
    scale_pool(NrWorkers + 1);

scale_pool(NrWorkers) when NrWorkers == 0 -> 
    ok.