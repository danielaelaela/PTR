-module(lab1_worker_spec).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) -> 
    % io:format("~p: ~p~n", ["Worker Spec", self()]),
    Prob = rand:uniform(),
    case Prob < 0.05 of
        true -> MSec = 10000;
        false -> MSec = round(2900 * rand:uniform() + 100)
    end,
    {ok, {self(), false, 0, MSec}}.


handle_call(_, _, _State) ->
    {noreply, _State}.

handle_cast(Tweet, {TaskPid, _, TaskNr, MSec}) ->
    stop_if_alive(TaskPid, erlang:is_process_alive(TaskPid)),  
    {ok, {NewTaskPid, _}} = gen_server:start_monitor(lab1_task, [Tweet, TaskNr, MSec, self()], []),
    {noreply, {NewTaskPid, Tweet, TaskNr + 1, MSec}}.

handle_info({_, _, _, _, normal}, {TaskPid, Tweet, TaskNr, MSec}) ->
    {noreply, {TaskPid, Tweet, TaskNr, MSec}};
handle_info({_, _, _, _, shutdown}, {TaskPid, Tweet, TaskNr, MSec}) ->
    lab1_router_spec:done(Tweet),
    {noreply, {TaskPid, Tweet, TaskNr, MSec}}.

stop_if_alive(Pid, true) when Pid =/= self() ->
    gen_server:stop(Pid);
stop_if_alive(Pid, true) when Pid == self() ->
    ok;
stop_if_alive(_, false) ->
    ok.
