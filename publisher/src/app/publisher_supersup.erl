-module(publisher_supersup).
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

    Publisher = #{
		id => lab3_publisher,
	    start => {lab3_publisher, start_link, []},
		restart => permanent, 
		shutdown => 2000, 
		type => worker,
	    modules => [lab3_publisher]
	},

    ChildSpecs = [Publisher],
    {ok, {SupFlags, ChildSpecs}}.
