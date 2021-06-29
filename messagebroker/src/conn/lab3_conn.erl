-module(lab3_conn).
-behaviour(gen_server).

-export([start_link/1, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([send_message/2, process/1, process_matched/2]).

start_link(Socket) ->
    gen_server:start_link(?MODULE, Socket, []).

init(Socket) ->
    io:format("~p: ~p~n", ["Connection", self()]),
    gen_server:cast(self(), {accept}),
    {ok, #{socket => Socket}}.

send_message(SubId, MessageInfo) ->
    io:format("~p: Here!~n", [self()]),
    gen_server:cast(SubId, {send, MessageInfo}).


handle_call(_, _, State) ->
    {noreply, State}.

handle_cast({accept}, State) ->
    #{socket := ListenSocket} = State,

    {ok, AcceptSocket} = gen_tcp:accept(ListenSocket),
    inet:setopts(AcceptSocket, [{active, once}]),

    lab3_conn_sup:start_socket(),

    {noreply, #{socket => AcceptSocket}};

handle_cast({send, MessageInfo}, State) ->
    
    io:format("~p: Here!!~n", [self()]),

    #{socket := Socket} = State,
    {Topic, Counter, Message} = MessageInfo,

    Message = #{
        type => message,
        param => #{
            topic => Topic,
            message => Message,
            counter => Counter
        }
    },

    io:format("~p: Here!~n", [self()]),

    Packet = serialize(Message),

    gen_tcp:send(Socket, Packet),
    inet:setopts(Socket, [{active, once}]),

    {noreply, State}.

handle_info({_, _, Packet}, State) ->
    #{socket := Socket} = State,

    Message = deserialize(Packet),
    Answer = process(Message),
    % Answer = Message,
    AnswerPacket = serialize(Answer),

    gen_tcp:send(Socket, AnswerPacket),
    inet:setopts(Socket, [{active, once}]),

    {noreply, State};

handle_info({tcp_closed, _}, _State) ->
    {stop, normal, _State}.


process(Message) -> 
    case Message of
        #{
            <<"type">> := <<"connect">>, 
            <<"param">> := #{
                <<"pubid">> := PubId,
                <<"topics">> := Topics
            }
        } -> process_matched({publisher, connect}, {PubId, Topics});

        #{
            <<"type">> := <<"disconnect">>, 
            <<"param">> := #{
                <<"pubid">> := PubId
            }
        } -> process_matched({publisher, disconnect}, {PubId});
    
        #{
            <<"type">> := <<"message">>, 
            <<"param">> := #{
                <<"topic">> := Topic,
                <<"message">> := Tweet
            }
        } -> process_matched({publisher, message}, {Topic, Tweet});

        #{
            <<"type">> := <<"connect">>, 
            <<"param">> := #{
                <<"subid">> := SubId
            }
        } -> process_matched({client, connect}, {SubId});

        #{
            <<"type">> := <<"disconnect">>, 
            <<"param">> := #{
                <<"subid">> := SubId
            }
        } -> process_matched({client, disconnect}, {SubId});

        #{
            <<"type">> := <<"subscribe">>, 
            <<"param">> := #{
                <<"subid">> := SubId,
                <<"topic">> := Topic
            }
        } -> process_matched({client, subscribe}, {SubId, Topic});

        #{
            <<"type">> := <<"unsubscribe">>, 
            <<"param">> := #{
                <<"subid">> := SubId,
                <<"topic">> := Topic
            }
        } -> process_matched({client, unsubscribe}, {SubId, Topic});

        #{
            <<"type">> := <<"ack">>, 
            <<"param">> := #{
                <<"subid">> := SubId,
                <<"topic">> := Topic
            }
        } -> process_matched({client, ack}, {SubId, Topic});

        #{
            <<"type">> := <<"lot">>
        } -> process_matched({client, lot}, {});

        Message -> process_matched({badmessage}, {Message})
    end.


process_matched({publisher, connect}, {PubId, Topics}) ->
    lab3_pubdb:connect({PubId, self(), Topics});

process_matched({publisher, disconnect}, {PubId}) ->
    lab3_pubdb:disconnect({PubId});

process_matched({publisher, message}, {Topic, Tweet}) ->
    lab3_tq:message(Topic, {Tweet});


process_matched({client, connect}, {SubId}) ->
    lab3_subdb:connect({SubId, self()});

process_matched({client, disconnect}, {SubId}) ->
    lab3_subdb:disconnect({SubId});

process_matched({client, subscribe}, {SubId, Topic}) ->
    lab3_subdb:subscribe({SubId, Topic});

process_matched({client, unsubscribe}, {SubId, Topic}) ->
    lab3_subdb:unsubscribe({SubId, Topic});

process_matched({client, ack}, {SubId, Topic}) ->
    lab3_tq:ack(Topic, {SubId});

process_matched({client, lot}, {}) ->
    lab3_tq_sup:list_of_topics();


process_matched({badmessage}, {BadMessage}) ->
    io:format("~p: Bad Message:~n~p~n", [?MODULE, BadMessage]),
    "BadMessage".


serialize(Message) ->
    jsx:encode(Message).

deserialize(Packet) ->
    NewPacket = list_to_binary(Packet),
    jsx:decode(NewPacket).
