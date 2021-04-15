-module(lab2_aggregator).
-behaviour(gen_server).

-export([start_link/0, init/1, aggregate/1, handle_call/3, handle_cast/2, search_id/2, handle_map/3, is_complete/1, handle_new_map/3]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Aggregator", self()]),
    {ok, []}.

aggregate(Data) ->
    gen_server:cast(?MODULE, {data, Data}).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({data, Data}, State) ->
    #{id := Id} = Data,

    IsId = search_id(Id, State),
    NewState = handle_map(IsId, Data, State),

    {noreply, NewState}.


search_id(Id, State) ->
    lists:search(fun(StateMap) ->
            #{id := StateId} = StateMap,
            Id =:= StateId
        end, 
        State
    ).

handle_map(IsId, Map, State) when IsId =:= false ->
    [Map | State];
handle_map(IsId, Map, State) when IsId =/= false ->
    {value, StateMap} = IsId,
    NewMap = maps:merge(StateMap, Map),
    TempState = lists:delete(StateMap, State),
    IsComplete = is_complete(NewMap),
    handle_new_map(IsComplete, NewMap, TempState).

handle_new_map(IsComplete, NewMap, State) when IsComplete =:= false ->
    [NewMap | State];
handle_new_map(IsComplete, NewMap, State) when IsComplete =:= true ->
    lab2_batcher:batch(NewMap),
    State.

is_complete(Map) ->
    maps:size(Map) =:= 4.
