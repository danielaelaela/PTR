-module(lab3_pubdb).
-behaviour(gen_server).

-export([start_link/0, init/1, connect/1, disconnect/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(PUBTABLE, pubtable).

-define(LOGTIME, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["PubDB", self()]),

    ets:new(?PUBTABLE, [set, named_table]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),
    {ok, {}}.

connect(ConnectInfo) ->
    gen_server:cast(?MODULE, {connect, ConnectInfo}).

disconnect(DisconnectInfo) ->
    gen_server:cast(?MODULE, {disconnect, DisconnectInfo}).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({connect, ConnectInfo}, _State) ->
    {PubId, Port, Topics} = ConnectInfo,

    ets:insert(?PUBTABLE, {PubId, Port, Topics}),
    lab3_tq_sup:ensure_tqs(Topics),

    {noreply, _State};

handle_cast({disconnect, DisconnectInfo}, _State) ->
    {PubId} = DisconnectInfo,

    ets:delete(?PUBTABLE, PubId),

    {noreply, _State}.


handle_info({timeout, _, _}, _State) ->
    % PubInfo = ets:info(?PUBTABLE),
    % {_, PubSize} = lists:keyfind(size, 1, PubInfo),
    % io:format("PubDB size: ~p events", [PubSize]),

    io:format("PubDB: ~p~n", [ets:tab2list(?PUBTABLE)]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),
    {noreply, _State}.


% separate_users(Batch) -> 
%     separate_users(Batch, {[], [], []}).
 
% separate_users([], {EventBatch, TweetBatch, UserBatch}) ->
%     {EventBatch, TweetBatch, UserBatch};
% separate_users([H|T], {EventBatch, TweetBatch, UserBatch}) ->
%     {Event, Tweet, User} = split(H),
%     separate_users(T, {[Event|EventBatch], [Tweet|TweetBatch], [User|UserBatch]}).

% split(Map) ->
%     #{
%         id := Id,
%         tweet := Twt,
%         ss := Ss,
%         er := Er
%     } = Map,

%     #{
%         <<"user">> := UserMap
%     } = Twt,
%     TweetMap = maps:without([<<"user">>], Twt),

%     TweetId = crypto:hash(md5, jsx:encode(TweetMap)),
%     UserId = crypto:hash(md5, jsx:encode(UserMap)),

%     Event = {Id, TweetId, UserId, Ss, Er},
%     Tweet = {TweetId, TweetMap},
%     User = {UserId, UserMap},
%     {Event, Tweet, User}.
