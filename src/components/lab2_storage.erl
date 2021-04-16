-module(lab2_storage).
-behaviour(gen_server).

-export([start_link/0, init/1, write_batch/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(EVENTTABLE, event).
-define(TWEETTABLE, tweet).
-define(USERTABLE, user).

-define(LOGTIME, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Storage", self()]),

    ets:new(?EVENTTABLE, [set, named_table]),
    ets:new(?TWEETTABLE, [set, named_table]),
    ets:new(?USERTABLE, [set, named_table]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),
    {ok, {}}.

write_batch(Batch) ->
    gen_server:cast(?MODULE, {batch, Batch}).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({batch, Batch}, _State) ->
    {EventBatch, TweetBatch, UserBatch} = separate_users(Batch), 

    ets:insert(?EVENTTABLE, EventBatch),
    ets:insert(?TWEETTABLE, TweetBatch),
    ets:insert(?USERTABLE, UserBatch),

    {noreply, _State}.

handle_info({timeout, _, _}, _State) ->
    EventInfo = ets:info(?EVENTTABLE),
    TweetInfo = ets:info(?TWEETTABLE),
    UserInfo = ets:info(?USERTABLE),

    {_, EventSize} = lists:keyfind(size, 1, EventInfo),
    {_, TweetSize} = lists:keyfind(size, 1, TweetInfo),
    {_, UserSize} = lists:keyfind(size, 1, UserInfo),


    io:format("Storage: ~p events, ~p tweets, ~p users~n", [EventSize, TweetSize, UserSize]),
  
    % io:format("User Storage: ~p~n", [ets:tab2list(?TWEETTABLE)]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),
    {noreply, _State}.


separate_users(Batch) -> 
    separate_users(Batch, {[], [], []}).
 
separate_users([], {EventBatch, TweetBatch, UserBatch}) ->
    {EventBatch, TweetBatch, UserBatch};
separate_users([H|T], {EventBatch, TweetBatch, UserBatch}) ->
    {Event, Tweet, User} = split(H),
    separate_users(T, {[Event|EventBatch], [Tweet|TweetBatch], [User|UserBatch]}).

split(Map) ->
    #{
        id := Id,
        tweet := Twt,
        ss := Ss,
        er := Er
    } = Map,

    #{
        <<"user">> := UserMap
    } = Twt,
    TweetMap = maps:without([<<"user">>], Twt),

    TweetId = crypto:hash(md5, jsx:encode(TweetMap)),
    UserId = crypto:hash(md5, jsx:encode(UserMap)),

    Event = {Id, TweetId, UserId, Ss, Er},
    Tweet = {TweetId, TweetMap},
    User = {UserId, UserMap},
    {Event, Tweet, User}.
