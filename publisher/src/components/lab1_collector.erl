-module(lab1_collector).
-behaviour(gen_server).

-export([start_link/2, init/1, handle_call/3, handle_cast/2, handle_info/2]).

start_link(Stream, PubId) ->
    gen_server:start_link(?MODULE, [Stream, PubId], []).

init([Stream, PubId]) ->
    io:format("~p: ~p~n", ["Collector", self()]),
    erlang:start_timer(0, self(), start),
    {ok, {Stream, PubId}}.


handle_call(_Request, _From, _State) ->
    {noreply, _State}.

handle_cast(_Request, _State) ->
    {noreply, _State}.

handle_info({_, _, start}, State) ->
    {Stream, PubId} = State,

    % lab2_filter:filter(<<"data: {\"message\": {\"tweet\": {\"retweeted_status\": \"Hello\"}} }">>, PubId),

    {ok, Conn} = shotgun:open("localhost", 4000),
    Options = #{
        async => true, 
        async_mode => sse,
        handle_event => fun (_, _, Event) -> lab2_filter:filter(Event, PubId) end
    },
    {ok, _Ref} = shotgun:get(Conn, Stream, #{}, Options),
    wait(100),
    shotgun:close(Conn),

    {noreply, State}.

wait(MSec) -> 
    receive  
    after 
        MSec -> ok 
    end.
