-module(lab1_task).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link([Tweet, TaskNr, MSec, WorkerPid]) -> 
    gen_server:start_link(?MODULE, [Tweet, TaskNr, MSec, WorkerPid], []).

init([Tweet, TaskNr, MSec, WorkerPid]) -> 
    % io:format("~p: ~p~n", ["Task", self()]),

    erlang:start_timer(MSec, self(), timeout),
    {ok, {Tweet, MSec, TaskNr, WorkerPid}}.


handle_call(_, _, _State) ->
    {noreply, _State}.

handle_cast(_, _State) ->
    {noreply, _State}.

handle_info({_, _, timeout}, {_Tweet, MSec, TaskNr, WorkerPid}) ->
    io:format("Worker ~p Task number: ~p, time: ~p~n", [WorkerPid, TaskNr, MSec]),
    {stop, shutdown, {}}.
