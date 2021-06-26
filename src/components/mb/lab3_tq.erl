-module(lab3_tq).
-behaviour(gen_server).

-export([start_link/1, init/1, message/2, subscribe/2, unsubscribe/2, ack/2, handle_call/3, handle_cast/2, handle_info/2]).

-define(LOGTIME, 2500).

start_link(Topic) ->
    gen_server:start_link({local, Topic}, ?MODULE, [Topic], []).

init([Topic]) ->
    io:format("~p ~p: ~p~n", ["TQ", Topic, self()]),

    ets:new(Topic, [set, named_table]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),

    InitialState = #{
        topic => Topic,
        counter => 1,
        pointers => #{}
    },
    {ok, InitialState}.


message(Topic, MessageInfo) ->
    gen_server:cast(Topic, {message, MessageInfo}).

subscribe(Topic, SubscribeInfo) ->
    gen_server:cast(Topic, {subscribe, SubscribeInfo}).

unsubscribe(Topic, UnsubscribeInfo) ->
    gen_server:cast(Topic, {unsubscribe, UnsubscribeInfo}).

ack(Topic, AckInfo) ->
    gen_server:cast(Topic, {ack, AckInfo}).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({message, MessageInfo}, State) ->
    {Message} = MessageInfo,
     #{
        topic := Topic,
        counter := Counter,
        pointers := Pointers
    } = State,

    ets:insert(Topic, {Counter, Message}),
    ToSendPointers = maps:filter(
        fun(_SubId, MessageId) -> 
            MessageId =:= Counter
        end,
        Pointers
    ),
    send_message(Topic, Counter + 1, Counter, maps:keys(ToSendPointers)),

    NewState = #{
        topic => Topic,
        counter => Counter + 1,
        pointers => Pointers
    },

    {noreply, NewState};

handle_cast({subscribe, SubscribeInfo}, State) ->
    {SubId} = SubscribeInfo,
    #{
        topic := Topic,
        counter := Counter,
        pointers := Pointers
    } = State,

    NewPointers = maps:put(SubId, Counter, Pointers),
    % send_message(Topic, Counter, Counter, [SubId]),

    NewState = #{
        topic => Topic,
        counter => Counter,
        pointers => NewPointers
    },
    {noreply, NewState};

handle_cast({unsubscribe, UnsubscribeInfo}, State) ->
    {SubId} = UnsubscribeInfo,
    #{
        topic := Topic,
        counter := Counter,
        pointers := Pointers
    } = State,

    NewPointers = maps:remove(SubId, Pointers),
    
    NewState = #{
        topic => Topic,
        counter => Counter,
        pointers => NewPointers
    },
    {noreply, NewState};

handle_cast({ack, AckInfo}, State) ->
    {SubId} = AckInfo,
    #{
        topic := Topic,
        counter := Counter,
        pointers := Pointers
    } = State,

    % NewPointers = maps:update_with(
    %     SubId, 
    %     fun(Id) -> 
    %         Id + 1 
    %     end, 
    %     Pointers
    % ),
    MessageId = maps:get(SubId, Pointers),
    NewPointers = maps:update(SubId, MessageId + 1, Pointers),
    send_message(Topic, Counter, MessageId + 1, [SubId]),

    NewState = #{
        topic => Topic,
        counter => Counter,
        pointers => NewPointers
    },

    {noreply, NewState}.

handle_info({timeout, _, _}, State) ->
    #{topic := Topic} = State,
    % PubInfo = ets:info(?PUBTABLE),
    % {_, PubSize} = lists:keyfind(size, 1, PubInfo),
    % io:format("PubDB size: ~p events", [PubSize]),

    io:format("TQ ~p: ~p~n", [Topic, ets:tab2list(Topic)]),
    io:format("State ~p: ~p~n", [Topic, State]),
    
    erlang:start_timer(?LOGTIME, self(), timeout),
    {noreply, State}.


send_message(Topic, Counter, MessageId, SubIds) when MessageId < Counter ->
    Message = ets:lookup_element(Topic, MessageId, 2),
    lists:map(
        fun(SubId) -> 
            % conn:please_send_message(Topic, Counter, Message, SubId)
            io:format("TQ ~p: Sending message ~p to ~p~n", [Topic, Message, SubId])
        end,
        SubIds
    );
send_message(_, _, _, _) ->
    ok.