-module(lab2_filter).
-behaviour(gen_server).

-export([start_link/0, init/1, filter/1, handle_call/3, handle_cast/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Filter", self()]),
    {ok, 0}.

filter(Event) ->
    gen_server:cast(?MODULE, {event, Event}).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({event, Event}, State) ->
    EventMap = shotgun:parse_event(Event),
    #{data := Data} = EventMap,
    IsJson = jsx:is_json(Data),

    process(Data, State, IsJson),
    {noreply, State + 2}.


process(Data, State, IsJson) when IsJson == true ->
    Json = jsx:decode(Data),
    #{
        <<"message">> := #{
            <<"tweet">> := Tweet
        }
    } = Json,

    Retweet = maps:get(<<"retweeted_status">>, Tweet, #{}),
    IdedTweets = add_ids(State, [Tweet, Retweet]),

    PoolSupER = lab1_supersup:get_child(lab1_pool_sup_er),
    PoolSupSS = lab1_supersup:get_child(lab1_pool_sup_ss),
    RouterER = lab1_pool_sup:get_child(PoolSupER, lab1_router),
    RouterSS = lab1_pool_sup:get_child(PoolSupSS, lab1_router),

    send(IdedTweets, RouterSS, RouterER);

process(_, _, IsJson) when IsJson == false ->
    ok.


add_ids(Id, Tweets) -> 
    add_ids(Id, Tweets, []).
 
add_ids(_, [], IdedTweets) ->
    IdedTweets;
add_ids(Id, [Tweet|T], IdedTweets) when Tweet =/= #{} -> 
    IdedTweet = #{id => Id, tweet => Tweet},
    add_ids(Id + 1, T, [IdedTweet|IdedTweets]);
add_ids(Id, [Tweet|T], IdedTweets) when Tweet =:= #{} -> 
    add_ids(Id, T, IdedTweets).


send([], _, _) ->
    ok;
send([IdedTweet|T], RouterSS, RouterER) ->
    lab1_router:route(RouterER, IdedTweet),
    lab1_router:route(RouterSS, IdedTweet),
    lab2_aggregator:aggregate(IdedTweet),
    % io:format("~p: ~p~n", [self(), IdedTweet]),
    send(T, RouterSS, RouterER).