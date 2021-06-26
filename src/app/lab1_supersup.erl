-module(lab1_supersup).
-behaviour(supervisor).

-export([start_link/0, init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    MaxRestart = 2,
    MaxTime = 100,
    SupFlags = #{
		strategy => one_for_all,
		intensity => MaxRestart, 
		period => MaxTime
	},

    WorkerSup = #{
		id => lab1_worker_sup,
	    start => {lab1_worker_sup, start_link, []},
	    restart => permanent, 
		shutdown => 2000,
	    type => supervisor, 
		modules => [lab1_worker_sup]
	},
    Scaler = #{
		id => lab1_scaler,
	    start => {lab1_scaler, start_link, []}, 
		restart => permanent,
	    shutdown => 2000, 
		type => worker, 
		modules => [lab1_scaler]
	},
    Router = #{
		id => lab1_router_spec,
	    start => {lab1_router_spec, start_link, []}, 
		restart => permanent,
	    shutdown => 2000, 
		type => worker, 
		modules => [lab1_router_spec]
	},
    Stream1 = "/tweets/1",
    Collector1 = #{id => lab1_collector1,
	      start => {lab1_collector, start, [Stream1]},
	      restart => permanent, shutdown => 2000, type => worker,
	      modules => [lab1_collector]},
    Stream2 = "/tweets/2",
    Collector2 = #{id => lab1_collector2,
	      start => {lab1_collector, start, [Stream2]},
	      restart => permanent, shutdown => 2000, type => worker,
	      modules => [lab1_collector]},

    ChildSpecs = [WorkerSup, Router, Collector1],
    % ChildSpecs = [WorkerSup, Scaler, Router, Collector1, Collector2],
    {ok, {SupFlags, ChildSpecs}}.