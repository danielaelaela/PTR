-module(lab2_batcher).
-behaviour(gen_server).

-export([start_link/0, init/1, batch/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(BATCHSIZE, 128).
-define(TIMELIMIT, 200).


start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Batcher", self()]),
    TimerRef = erlang:start_timer(?TIMELIMIT, self(), timeout),
    {ok, {[], TimerRef}}.

batch(Map) ->
    gen_server:cast(?MODULE, {map, Map}).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({map, Map}, State) ->
    {Batch, TimerRef} = State,
    NewBatch = [Map | Batch],
    erlang:start_timer(0, self(), limit),
    {noreply, {NewBatch, TimerRef}}.

handle_info({_, _, limit}, State) ->
    {Batch, TimerRef} = State,
    case length(Batch) of
        ?BATCHSIZE -> 
            lab2_storage:write_batch(Batch),
            io:format("Batch sent from limit: ~p~n", [length(Batch)]),
            erlang:cancel_timer(TimerRef, []), 
            NewTimerRef = erlang:start_timer(5000, self(), timeout),  
            {noreply, {[], NewTimerRef}};
        _ ->
            {noreply, State}
    end;
handle_info({_, _, timeout}, State) ->
    {Batch, _} = State,
    lab2_storage:write_batch(Batch),
    io:format("Batch sent from timeout: ~p~n", [length(Batch)]),

    NewTimerRef = erlang:start_timer(?TIMELIMIT, self(), timeout),

    {noreply, {[], NewTimerRef}}.
