-module(lab1_worker5).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2]).

start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) -> 
    io:format("~p: ~p~n", ["Worker5", self()]),
    {ok, {}}.

handle_cast(Tweet, _State) ->
    IsPanic = detect_panic(Tweet),
    if
        IsPanic ->
            io:format("~p: ~p~n", [self(), "Panic"]),
            exit(panic);
        true ->
            sleep(),
            io:format("~p: ~p~n", [self(), bit_size(Tweet)])
    end, 
    {noreply, _State}.


detect_panic(Tweet) ->
    Tweet == "Panic".

sleep() ->
    MSec = round(450 * rand:uniform() + 50),
    % wait(1000).
    wait(MSec).

wait(MSec) -> 
    receive  
    after 
        MSec -> ok 
    end.
