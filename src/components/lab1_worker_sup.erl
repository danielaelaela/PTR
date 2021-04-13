-module(lab1_worker_sup).
-behaviour(supervisor).

-export([start_link/1, init/1, start_worker/1, stop_worker/2]).

start_link(WorkerType) ->
    supervisor:start_link(?MODULE, [WorkerType]).

init([WorkerType]) ->
    io:format("~p: ~p~n", ["Worker Supervisor", self()]),
    
    MaxRestart = 6,
    MaxTime = 3600,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestart,
        period => MaxTime
    },

    ChildSpec = get_child_spec(WorkerType),

    {ok, {SupFlags, [ChildSpec]}}.

get_child_spec(worker) ->
    #{
        id => lab1_worker,
        start => {lab1_worker, start_link, []}, 
        restart => permanent,
        shutdown => 2000, 
        type => worker
    };
get_child_spec(worker5) ->
    #{
        id => lab1_worker5,
        start => {lab1_worker5, start_link, []}, 
        restart => permanent,
        shutdown => 2000, 
        type => worker
    };
get_child_spec(_) ->
    io:format("~p: ~p~n", [self(), "Invalid WorkerType!!!"]),
    #{}.

start_worker(WorkerSup) ->
    supervisor:start_child(WorkerSup, []).

stop_worker(WorkerSup, WorkerPid) ->
    supervisor:terminate_child(WorkerSup, WorkerPid).
