-module(lab1_worker_ss).
-behaviour(gen_server).

-export([start_link/0, init/1, work/2, handle_call/3, handle_cast/2]).

start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) -> 
    io:format("~p: ~p~n", ["Worker SS", self()]),
    {ok, {}}.

work(Pid, IdedTweet) ->
    gen_server:cast(Pid, {work, IdedTweet}).


handle_call(_Request, _From, _State) ->
    {noreply, _State}.

handle_cast({work, IdedTweet}, State) ->
    sleep(),
    #{id := Id, tweet := Tweet} = IdedTweet,
    #{
        <<"text">> := Text
    } = Tweet,
    TweetSS = compute(Text),
    lab2_aggregator:aggregate(#{id => Id, ss => TweetSS}),
    % io:format("~p: id ~p tweetSS ~p~n", [self(), Id, TweetSS]),
    {noreply, State}.


compute(Text) ->
    TextList = unicode:characters_to_list(Text, utf8),
    LowText = string:lowercase(TextList),
    Tokens = string:tokens(LowText, "\n ,.?!;:/'"),
    Sum = lists:sum(lists:map(
        fun (Word) -> lab1_emotional_score:get_score(Word) end,
		Tokens
    )),
    Sum / length(Tokens).

sleep() ->
    MSec = round(450 * rand:uniform() + 50),
    wait(MSec).

wait(MSec) -> 
    receive  
    after 
        MSec -> ok 
    end.
