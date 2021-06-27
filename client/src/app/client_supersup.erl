-module(client_supersup).
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

    Client = #{
		id => lab3_client,
	    start => {lab3_client, start_link, []},
		restart => permanent, 
		shutdown => 2000, 
		type => worker,
	    modules => [lab3_client]
	},

    ChildSpecs = [Client],
    {ok, {SupFlags, ChildSpecs}}.
