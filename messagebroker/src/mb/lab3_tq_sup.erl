-module(lab3_tq_sup).
-behaviour(supervisor).

-export([start_link/0, init/1, ensure_tqs/1, list_of_topics/0]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    io:format("~p: ~p~n", ["TQ Sup", self()]),
    MaxRestart = 6,
    MaxTime = 3600,
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => MaxRestart,
        period => MaxTime
    },

	TQ = #{
		id => lab3_tq,
	    start => {lab3_tq, start_link, []},
	    restart => permanent, 
		shutdown => 2000,
	    type => worker, 
		modules => [lab3_tq]
	},
    
    ChildSpecs = [TQ],
    {ok, {SupFlags, ChildSpecs}}.

ensure_tqs(Topics) ->
    ExistingTopics = list_of_topics(),
    lists:map(
        fun(Topic) -> 
            ensure_tq(Topic, ExistingTopics)
        end,
        Topics
    ).

    ensure_tq(Topic, ExistingTopics) ->
        Exists = lists:any(
            fun(ExistingTopic) ->
                Topic =:= ExistingTopic
            end,
            ExistingTopics    
        ),
        create_tq(Topic, Exists).

    create_tq(_, true) ->
        ok;
    create_tq(Topic, false) ->
        supervisor:start_child(?MODULE, [Topic]).

list_of_topics() ->
    Children = supervisor:which_children(?MODULE),
    lists:map(
        fun({_, Pid, _, _}) -> 
            {registered_name, Topic} = process_info(Pid, registered_name),
            Topic
        end,
        Children
    ).

% stop_worker(WorkerSup, WorkerPid) ->
%     supervisor:terminate_child(WorkerSup, WorkerPid).
