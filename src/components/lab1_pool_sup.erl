-module(lab1_pool_sup).
-behaviour(supervisor).

-export([start_link/1, init/1, get_child/2]).

start_link(WorkerType) ->
    supervisor:start_link(?MODULE, [WorkerType]).

init([WorkerType]) ->
    MaxRestart = 2,
    MaxTime = 100,
    SupFlags = #{
		strategy => one_for_all,
		intensity => MaxRestart, 
		period => MaxTime
	},

	Scaler = #{
		id => lab1_scaler,
	    start => {lab1_scaler, start_link, [self()]}, 
		restart => permanent,
	    shutdown => 2000, 
		type => worker, 
		modules => [lab1_scaler]
	},

    Router = #{
		id => lab1_router,
	    start => {lab1_router, start_link, [self()]}, 
		restart => permanent,
	    shutdown => 2000, 
		type => worker, 
		modules => [lab1_router]
	},

	WorkerSup = #{
		id => lab1_worker_sup,
	    start => {lab1_worker_sup, start_link, [WorkerType]},
	    restart => permanent, 
		shutdown => 2000,
	    type => supervisor, 
		modules => [lab1_worker_sup]
	},
    
    ChildSpecs = [WorkerSup, Router, Scaler],
    {ok, {SupFlags, ChildSpecs}}.


get_child(PoolSup, SiblingId) ->
    Siblings = supervisor:which_children(PoolSup),
    {value, {_, Child, _, _}} = lists:search(
        fun(Sibling) ->
            {Id, _, _, _} = Sibling,
            SiblingId =:= Id
        end, 
        Siblings
    ),
    Child.