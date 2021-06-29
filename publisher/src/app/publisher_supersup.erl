-module(publisher_supersup).
-behaviour(supervisor).

-export([start_link/0, init/1]).
-export([start_collector/1]).

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

    Filter = #{
        id => lab2_filter,
        start => {lab2_filter, start_link, []}, 
        restart => permanent,
        shutdown => 2000, 
        type => worker, 
        modules => [lab2_filter]
    },

    ChildSpecs = [Publisher, Filter],
    {ok, {SupFlags, ChildSpecs}}.


start_collector(PubId) ->
    Stream1 = "/tweets/1",
    Collector1 = #{
        id => lab1_collector1,
        start => {lab1_collector, start_link, [Stream1, PubId]},
        restart => permanent, 
        shutdown => 2000, 
        type => worker,
        modules => [lab1_collector]
    },
    supervisor:start_child(?MODULE, Collector1).
