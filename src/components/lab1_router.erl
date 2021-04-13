-module(lab1_router).
-behaviour(gen_server).

-export([start_link/1, init/1, route/2, handle_cast/2]).


start_link(Parent) ->
    gen_server:start_link(?MODULE, [Parent], []).

init([Parent]) ->
    io:format("~p: ~p~n", ["Router", self()]),
    {ok, {0, Parent}}.

route(Pid, Tweet) ->
    gen_server:cast(Pid, {tweet, Tweet}).

handle_cast({tweet, Tweet}, State) ->
    {WorkerIdx, Parent} = State,
    Scaler = lab1_pool_sup:get_child(Parent, lab1_scaler),
    WorkerSup = lab1_pool_sup:get_child(Parent, lab1_worker_sup),

    lab1_scaler:scale(Scaler),

    WorkerPids = supervisor:which_children(WorkerSup),
    TotalChildren = length(WorkerPids),

    {_, WorkerPid, _, _} = lists:nth((WorkerIdx rem TotalChildren) + 1, WorkerPids),
    gen_server:cast(WorkerPid, Tweet),
    
    NewWorkerIdx = (WorkerIdx + 1) rem TotalChildren,
    {noreply, {NewWorkerIdx, Parent}}.
