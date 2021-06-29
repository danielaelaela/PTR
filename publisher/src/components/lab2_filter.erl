-module(lab2_filter).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2]).
-export([filter/2]).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Filter", self()]),
    {ok, []}.

filter(Event, PubId) ->
    gen_server:cast(?MODULE, {event, Event, PubId}).


handle_call(_, _, _State) ->
    {noreply, _State}.

handle_cast({event, Event, PubId}, _State) ->
    EventMap = shotgun:parse_event(Event),
    #{data := Data} = EventMap,
    IsJson = jsx:is_json(Data),

    process(Data, IsJson, PubId),
    % timer:sleep(100),
    
    {noreply, _State}.


process(Data, IsJson, PubId) when IsJson == true ->
    Json = jsx:decode(Data),
    #{
        <<"message">> := #{
        <<"tweet">> := Tweet
        }
    } = Json,
    Retweet = maps:get(<<"retweeted_status">>, Tweet, #{}),

    lab3_publisher:send_message(PubId, tweet, Tweet),
    lab3_publisher:send_message(PubId, retweet, Retweet);

process(_, IsJson, _) when IsJson == false ->
    ok.
