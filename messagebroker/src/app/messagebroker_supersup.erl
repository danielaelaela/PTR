-module(messagebroker_supersup).
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

    ConnSup = #{
		id => lab3_conn_sup,
	    start => {lab3_conn_sup, start_link, []},
	    restart => permanent, 
		shutdown => 2000,
	    type => supervisor, 
		modules => [lab3_conn_sup]
	},

    PubDB = #{
		id => lab3_pubdb,
	    start => {lab3_pubdb, start_link, []},
		restart => permanent, 
		shutdown => 2000, 
		type => worker,
	    modules => [lab3_pubdb]
	},

    SubDB = #{
		id => lab3_subdb,
	    start => {lab3_subdb, start_link, []},
		restart => permanent, 
		shutdown => 2000, 
		type => worker,
	    modules => [lab3_subdb]
	},

	TQSup = #{
		id => lab3_tq_sup,
	    start => {lab3_tq_sup, start_link, []},
	    restart => permanent, 
		shutdown => 2000,
	    type => supervisor, 
		modules => [lab3_tq_sup]
	},

    ChildSpecs = [TQSup, PubDB, SubDB, ConnSup],
    {ok, {SupFlags, ChildSpecs}}.
