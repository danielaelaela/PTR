-module(lab2_batcher).
-behaviour(gen_server).

-export([start_link/0, init/1, write_sscore/1, handle_cast/2, handle_info/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Batcher", self()]),
    TimerRef = erlang:start_timer(5000, self(), timeout),
    {ok, {[], TimerRef}}.

write_sscore(SScore) ->
    gen_server:cast(?MODULE, {sscore, SScore}).

handle_cast({sscore, SScore}, State) ->
    {Batch, TimerRef} = State,
    NewBatch = [SScore | Batch],
    erlang:start_timer(0, self(), limit),
    {noreply, {NewBatch, TimerRef}}.

handle_info({_, _, limit}, State) ->
    {Batch, TimerRef} = State,
    case length(Batch) of
        3 -> 
            % lab2_storage:write_batch(State),
            io:format("Batch sent from limit: ~p~n", [Batch]),
            erlang:cancel_timer(TimerRef, []), 
            NewTimerRef = erlang:start_timer(5000, self(), timeout),  
            {noreply, {[], NewTimerRef}};
        _ ->
            {noreply, State}
    end;
handle_info({_, _, timeout}, State) ->
    {Batch, TimerRef} = State,
%    lab2_storage:write_batch(State),
    io:format("Batch sent: ~p~n", [Batch]),

    NewTimerRef = erlang:start_timer(5000, self(), timeout),

    {noreply, {[], NewTimerRef}}.
