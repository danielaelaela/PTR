-module(lab1_router_spec).
-behaviour(gen_server).

-export([start_link/0, init/1, route/1, done/1, handle_call/3, handle_cast/2, handle_info/2, handle_continue/2]).

-define(WORKER_SUP, lab1_worker_sup).
-define(SCALER, lab1_scaler).
-define(RETRYTIME, 100).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    timer:sleep(1000),
    io:format("~p: ~p~n", ["Router Spec", self()]),

    WorkerPids = lists:map(
        fun({_, WorkerPid, _, _}) ->
            {WorkerPid, false}
        end, 
        supervisor:which_children(?WORKER_SUP)
    ),
    Workers = maps:from_list(WorkerPids),
    {ok, {Workers}}.

route(Tweet) ->
    gen_server:cast(?MODULE, {tweet, Tweet}),
    gen_server:cast(?MODULE, {tweet, Tweet}),
    gen_server:cast(?MODULE, {tweet, Tweet}).

done(Tweet) ->
    gen_server:cast(?MODULE, {done, Tweet}).


handle_call(_, _, _State) ->
    {noreply, _State}.

handle_cast({tweet, Tweet}, {Workers}) ->
    {noreply, {Workers}, {continue, {tweet, Tweet}}};

handle_cast({done, Tweet}, {Workers}) ->
    % Find Workers working with Tweet; Free them
    FreedWorkers = maps:filtermap(
        fun(_, Value) when Value == Tweet ->
            {true, false};
        (_, _) ->
            false
        end, 
        Workers
    ),

    NewWorkers = maps:merge(Workers, FreedWorkers),
    {noreply, {NewWorkers}}.

handle_continue({tweet, Tweet}, {Workers}) ->    
    % Find free worker
    WorkerPid = free_worker(Workers),
    continue(Workers, Tweet, WorkerPid).

handle_info({timeout, _, Tweet}, {Workers}) ->
    {noreply, {Workers}, {continue, {tweet, Tweet}}}.


continue(Workers, Tweet, false) ->
    erlang:start_timer(?RETRYTIME, self(), Tweet),
    {noreply, {Workers}};
continue(Workers, Tweet, WorkerPid) ->
    % Route Tweet to Worker
    gen_server:cast(WorkerPid, Tweet),

    % Mark Worker as busy
    NewWorkers = maps:update(WorkerPid, Tweet, Workers),
    {noreply, {NewWorkers}}.    

free_worker(Workers) ->
    FreeWorkers = maps:filter(
        fun(_, false) ->
            true;
        (_, _) ->
            false
        end, 
        Workers
    ),
    free(FreeWorkers, maps:size(FreeWorkers)).

free(_, 0) ->
    false;
free(Workers, _) ->
    [Head | _] = maps:keys(Workers),
    Head.
