-module(lab1_worker_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_worker/0, stop_worker/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("~p: ~p~n", ["Worker Supervisor", self()]),
    MaxRestart = 6,
    MaxTime = 3600,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestart,
        period => MaxTime
    },
    Worker = #{
        id => lab1_worker,
        start => {lab1_worker, start_link, []}, 
        restart => permanent,
        shutdown => 2000, 
        type => worker
    },
    {ok, {SupFlags, [Worker]}}.

start_worker() ->
    supervisor:start_child(?MODULE, []).

stop_worker(WorkerPid) ->
    supervisor:terminate_child(?MODULE, WorkerPid).
