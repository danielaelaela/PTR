-module(lab2_filter).
-behaviour(gen_server).

-export([start_link/0, init/1, filter/1, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Filter", self()]),
    {ok, 0}.

filter(Event) ->
    gen_server:cast(?MODULE, {event, Event}).


handle_cast({event, Event}, State) ->
    EventMap = shotgun:parse_event(Event),
    #{data := Data} = EventMap,
    IsJson = jsx:is_json(Data),

    process(Data, State, IsJson),
    {noreply, State + 1}.

process(Data, State, IsJson) when IsJson == true ->
    Json = jsx:decode(Data),
    #{<<"message">> := #{<<"tweet">> := Tweet}} = Json,
    IdedTweet = add_id(State, Tweet),

    PoolSupER = lab1_supersup:get_child(lab1_pool_sup_er),
    PoolSupSS = lab1_supersup:get_child(lab1_pool_sup_ss),
    RouterER = lab1_pool_sup:get_child(PoolSupER, lab1_router),
    RouterSS = lab1_pool_sup:get_child(PoolSupSS, lab1_router),

    lab1_router:route(RouterER, IdedTweet),
    lab1_router:route(RouterSS, IdedTweet),
    % lab2_aggregator:aggregate(IdedTweet),
    % io:format("~p: ~p~n", [self(), IdedTweet]),
    ok;
process(_, _, IsJson) when IsJson == false ->
    ok.

add_id(State, Tweet) ->
    {State, Tweet}.



