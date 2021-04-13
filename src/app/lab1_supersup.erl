-module(lab1_supersup).
-behaviour(supervisor).

-export([start_link/0, init/1, get_child/1]).

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

    Filter = #{
		id => lab2_filter,
	    start => {lab2_filter, start_link, []}, 
		restart => permanent,
	    shutdown => 2000, 
		type => worker, 
		modules => [lab2_filter]
	},

	PoolSupER = #{
		id => lab1_pool_sup_er,
	    start => {lab1_pool_sup, start_link, [worker5]},
	    restart => permanent, 
		shutdown => 2000,
	    type => supervisor, 
		modules => [lab1_pool_sup]
	},

	PoolSupSS = #{
		id => lab1_pool_sup_ss,
	    start => {lab1_pool_sup, start_link, [worker]},
	    restart => permanent, 
		shutdown => 2000,
	    type => supervisor, 
		modules => [lab1_pool_sup]
	},

    Stream1 = "/tweets/1",
    Collector1 = #{
		id => lab1_collector1,
	    start => {lab1_collector, start_link, [Stream1]},
		restart => permanent, 
		shutdown => 2000, 
		type => worker,
	    modules => [lab1_collector]
	},

    Stream2 = "/tweets/2",
    Collector2 = #{
		id => lab1_collector2,
	    start => {lab1_collector, start_link, [Stream2]},
		restart => permanent, 
		shutdown => 2000, 
		type => worker,
	    modules => [lab1_collector]
	},

    ChildSpecs = [PoolSupER, PoolSupSS, Filter, Collector1, Collector2],
    {ok, {SupFlags, ChildSpecs}}.


get_child(SiblingId) ->
    Siblings = supervisor:which_children(?MODULE),
    {value, {_, Child, _, _}} = lists:search(
        fun(Sibling) ->
            {Id, _, _, _} = Sibling,
            SiblingId =:= Id
        end, 
        Siblings
    ),
    Child.