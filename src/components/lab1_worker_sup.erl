-module(lab1_worker_sup).
-behaviour(supervisor).

-export([start_link/1, init/1, start_worker/1, stop_worker/2]).

start_link(WorkerType) ->
    supervisor:start_link(?MODULE, [WorkerType]).

init([WorkerType]) ->    
    MaxRestart = 6,
    MaxTime = 3600,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestart,
        period => MaxTime
    },

    ChildSpec = get_child_spec(WorkerType),
    {ok, {SupFlags, [ChildSpec]}}.

get_child_spec(workerSS) ->
    #{
        id => lab1_worker_ss,
        start => {lab1_worker_ss, start_link, []}, 
        restart => permanent,
        shutdown => 2000, 
        type => worker
    };
get_child_spec(workerER) ->
    #{
        id => lab2_worker_er,
        start => {lab2_worker_er, start_link, []}, 
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
