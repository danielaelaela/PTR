-module(lab3_conn_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, start_socket/0]).

-define(PORT, 8544).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("~p: ~p~n", ["Connection Sup", self()]),
    MaxRestart = 60,
    MaxTime = 3600,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestart,
        period => MaxTime
    },

    {ok, ListenSocket} = gen_tcp:listen(?PORT,[{active, false}, {packet, 2}]),
    spawn_link(fun first_children/0),

	Conn = #{
		id => lab3_conn,
	    start => {lab3_conn, start_link, [ListenSocket]}, 
		restart => temporary,
	    shutdown => 1000, 
		type => worker, 
		modules => [lab3_conn]
	},

    ChildSpecs = [Conn],
    {ok, {SupFlags, ChildSpecs}}.


start_socket() ->
    supervisor:start_child(?MODULE, []).

first_children() ->
    lists:map(
        fun(_) ->
            start_socket()
        end,
        lists:seq(1,3)
    ),
    ok.
