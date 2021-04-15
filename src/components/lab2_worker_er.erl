-module(lab2_worker_er).
-behaviour(gen_server).

-export([start_link/0, init/1, work/2, handle_call/3, handle_cast/2, compute/3]).

start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) -> 
    io:format("~p: ~p~n", ["Worker ER", self()]),
    {ok, {}}.

work(Pid, IdedTweet) ->
    gen_server:cast(Pid, {work, IdedTweet}).


handle_call(_Request, _From, _State) ->
    {noreply, _State}.

handle_cast({work, IdedTweet}, State) ->
    sleep(),
    #{
        id := Id, 
        tweet := Tweet
    } = IdedTweet,
    #{
        <<"retweet_count">> := Retweets,
        <<"favorite_count">> := Favourites,
        <<"user">> := #{
            <<"followers_count">> := Followers
        }
    } = Tweet,
    TweetER = compute(Favourites, Retweets, Followers),
    lab2_aggregator:aggregate(#{id => Id, er => TweetER}),
    % io:format("~p: id ~p tweetER ~p~n", [self(), Id, TweetER]),
    {noreply, State}.


compute(Favourites, Retweets, Followers) when Followers =/= 0 ->
    (Favourites + Retweets) / Followers;
compute(_, _, Followers) when Followers =:= 0 ->    
    0.

sleep() ->
    MSec = round(450 * rand:uniform() + 50),
    wait(MSec).

wait(MSec) -> 
    receive  
    after 
        MSec -> ok 
    end.