-module(lab3_tq).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([message/2, subscribe/2, unsubscribe/2, ack/2]).

start_link(Topic) ->
    gen_server:start_link({local, Topic}, ?MODULE, [Topic], []).

init([Topic]) ->
    io:format("~p ~p: ~p~n", ["TQ", Topic, self()]),

    ets:new(Topic, [set, named_table]),
    
    InitialState = #{
        topic => Topic,
        counter => 1,
        pointers => #{}
    },
    {ok, InitialState}.


message(Topic, MessageInfo) ->
    gen_server:call(Topic, {message, MessageInfo}).

subscribe(Topic, SubscribeInfo) ->
    gen_server:cast(Topic, {subscribe, SubscribeInfo}).

unsubscribe(Topic, UnsubscribeInfo) ->
    gen_server:cast(Topic, {unsubscribe, UnsubscribeInfo}).

ack(Topic, AckInfo) ->
    gen_server:cast(Topic, {ack, AckInfo}).


handle_call({message, MessageInfo}, _, State) ->
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

    {reply, "Message Saved", NewState}.

handle_cast({subscribe, SubscribeInfo}, State) ->
    {SubId} = SubscribeInfo,
    #{
        topic := Topic,
        counter := Counter,
        pointers := Pointers
    } = State,

    NewPointers = maps:put(SubId, Counter, Pointers),

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

    MessageId = maps:get(SubId, Pointers),
    NewPointers = maps:update(SubId, MessageId + 1, Pointers),
    send_message(Topic, Counter, MessageId + 1, [SubId]),

    NewState = #{
        topic => Topic,
        counter => Counter,
        pointers => NewPointers
    },
    {noreply, NewState}.


handle_info(status, State) ->
    #{topic := Topic} = State,
    io:format("TQ ~p: ~p~n", [Topic, ets:tab2list(Topic)]),
    io:format("State ~p: ~p~n", [Topic, State]),
    {noreply, State};

handle_info(_, State) ->
    #{topic := Topic} = State,
    io:format("TQ ~p: what?~n", [Topic]),
    {noreply, State}.


send_message(Topic, Counter, MessageId, SubIds) when MessageId < Counter ->
    Message = ets:lookup_element(Topic, MessageId, 2),
    lists:map(
        fun(SubId) -> 
            lab3_conn:send_message(SubId, {Topic, MessageId, Message})
            % io:format("TQ ~p: Sending message ~p to ~p~n", [Topic, Message, SubId])
        end,
        SubIds
    );
send_message(_, _, _, _) ->
    ok.