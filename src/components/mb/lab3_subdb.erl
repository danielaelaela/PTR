-module(lab3_subdb).
-behaviour(gen_server).

-export([start_link/0, init/1, connect/1, subscribe/1, unsubscribe/1, disconnect/1, handle_call/3, handle_cast/2, handle_info/2]).

-define(SUBTABLE, subtable).

-define(LOGTIME, 1000).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["SubDB", self()]),

    ets:new(?SUBTABLE, [set, named_table]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),
    {ok, {}}.

connect(ConnectInfo) ->
    gen_server:cast(?MODULE, {connect, ConnectInfo}).

subscribe(SubscribeInfo) ->
    gen_server:cast(?MODULE, {subscribe, SubscribeInfo}).

unsubscribe(UnsubscribeInfo) ->
    gen_server:cast(?MODULE, {unsubscribe, UnsubscribeInfo}).

disconnect(DisconnectInfo) ->
    gen_server:cast(?MODULE, {disconnect, DisconnectInfo}).



handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({connect, ConnectInfo}, _State) ->
    {SubId, Port} = ConnectInfo,

    ets:insert(?SUBTABLE, {SubId, Port, []}),

    {noreply, _State};

handle_cast({subscribe, SubscribeInfo}, _State) ->
    {SubId, Topic} = SubscribeInfo,

    % subscribe 1.1
    Topics = ets:lookup_element(?SUBTABLE, SubId, 3),
    ets:update_element(?SUBTABLE, SubId, {3, [Topic | Topics]}),

    % subscribe 1.2
    % lab3_tq:create_pointer(SubId, Topic),

    {noreply, _State};

handle_cast({unsubscribe, UnsubscribeInfo}, _State) ->
    {SubId, Topic} = UnsubscribeInfo,

    custom_unsubscribe(SubId, Topic),

    {noreply, _State};

handle_cast({disconnect, DisconnectInfo}, _State) ->
    {SubId} = DisconnectInfo,

    Topics = ets:lookup_element(?SUBTABLE, SubId, 3),
    lists:map(
        fun(Topic) -> 
            custom_unsubscribe(SubId, Topic)
        end,
        Topics
    ),

    ets:delete(?SUBTABLE, SubId),

    {noreply, _State}.


handle_info({timeout, _, _}, _State) ->
    % PubInfo = ets:info(?PUBTABLE),
    % {_, PubSize} = lists:keyfind(size, 1, PubInfo),
    % io:format("PubDB size: ~p events", [PubSize]),

    io:format("SubDB: ~p~n", [ets:tab2list(?SUBTABLE)]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),
    {noreply, _State}.



custom_unsubscribe(SubId, Topic) ->
    % unsubscribe 1.1
    Topics = ets:lookup_element(?SUBTABLE, SubId, 3),
    ets:update_element(?SUBTABLE, SubId, {3, lists:delete(Topic, Topics)}),

    % unsubcribe 1.2
    % lab3_tq:remove_pointer(SubId, Topic),
    io:format("Unsubbed: ~p from ~p~n", [SubId, Topic]),

    ok.