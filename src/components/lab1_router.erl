-module(lab1_router).
-behaviour(gen_server).

-export([start_link/0, init/1, route/1, handle_cast/2]).

-define(WORKER_SUP, lab1_worker_sup).
-define(SCALER, lab1_scaler).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Router", self()]),
    {ok, 0}.

route(Tweet) ->
    ?SCALER:routed(),
    gen_server:cast(?MODULE, {tweet, Tweet}).

handle_cast({tweet, Tweet}, State) ->
    WorkerPids = supervisor:which_children(?WORKER_SUP),
    TotalChildren = length(WorkerPids),

    {_, WorkerPid, _, _} = lists:nth((State rem TotalChildren) + 1, WorkerPids),
    gen_server:cast(WorkerPid, Tweet),
    
    NewState = (State + 1) rem TotalChildren,
    {noreply, NewState}.
