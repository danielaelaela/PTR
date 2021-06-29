-module(lab3_subdb).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([connect/1, subscribe/1, unsubscribe/1, disconnect/1]).

-define(SUBTABLE, subtable).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["SubDB", self()]),

    ets:new(?SUBTABLE, [set, named_table]),
    
    {ok, {}}.

connect(ConnectInfo) ->
    gen_server:call(?MODULE, {connect, ConnectInfo}).

subscribe(SubscribeInfo) ->
    gen_server:call(?MODULE, {subscribe, SubscribeInfo}).

unsubscribe(UnsubscribeInfo) ->
    gen_server:call(?MODULE, {unsubscribe, UnsubscribeInfo}).

disconnect(DisconnectInfo) ->
    gen_server:call(?MODULE, {disconnect, DisconnectInfo}).


handle_call({connect, ConnectInfo}, _, _State) ->
    {SubId, Port} = ConnectInfo,

    ets:insert(?SUBTABLE, {SubId, Port, []}),

    {reply, "Connected", _State};

handle_call({subscribe, SubscribeInfo}, _, _State) ->
    {SubId, Topic} = SubscribeInfo,

    % subscribe 1.1
    Topics = ets:lookup_element(?SUBTABLE, SubId, 3),
    ets:update_element(?SUBTABLE, SubId, {3, [Topic | Topics]}),

    % subscribe 1.2
    Port = ets:lookup_element(?SUBTABLE, SubId, 2),
    lab3_tq:subscribe(Topic, {Port}),

    {reply, "Subscribed", _State};

handle_call({unsubscribe, UnsubscribeInfo}, _, _State) ->
    {SubId, Topic} = UnsubscribeInfo,

    custom_unsubscribe(SubId, Topic),

    {reply, "Unsubscribed", _State};

handle_call({disconnect, DisconnectInfo}, _, _State) ->
    {SubId} = DisconnectInfo,

    Topics = ets:lookup_element(?SUBTABLE, SubId, 3),
    lists:map(
        fun(Topic) -> 
            custom_unsubscribe(SubId, Topic)
        end,
        Topics
    ),
    ets:delete(?SUBTABLE, SubId),

    {reply, "Disconnected", _State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(status, _State) ->
    io:format("SubDB: ~p~n", [ets:tab2list(?SUBTABLE)]),
    {noreply, _State};

handle_info(_, _State) ->
    io:format("SubDB: what?~n", []),
    {noreply, _State}.


custom_unsubscribe(SubId, Topic) ->
    % unsubscribe 1.1
    Topics = ets:lookup_element(?SUBTABLE, SubId, 3),
    ets:update_element(?SUBTABLE, SubId, {3, lists:delete(Topic, Topics)}),

    % unsubcribe 1.2
    Port = ets:lookup_element(?SUBTABLE, SubId, 2),
    lab3_tq:unsubscribe(Topic, {Port}),
    ok.
