-module(lab3_publisher).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([send_connect/2, send_disconnect/1, send_message/3, start_collector/1]).

-define(PORT, 8544).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["Publisher", self()]),
    {ok, Socket} = gen_tcp:connect(
        "localhost", 
        ?PORT, 
        [{active, true}, {packet, 2}]
    ),
    {ok, #{socket => Socket}}.


send_connect(PubId, Topics) ->
    Message = #{
        type => connect,
        param => #{
            pubid => PubId,
            topics => Topics
        }
    },
    gen_server:cast(?MODULE, {send, Message}).

send_disconnect(PubId) ->
    Message = #{
        type => disconnect,
        param => #{
            pubid => PubId
        }
    },
    gen_server:cast(?MODULE, {send, Message}).

send_message(PubId, Topic, Tweet) ->
    Message = #{
        type => message,
        param => #{
            pubid => PubId,
            topic => Topic,
            message => Tweet
        }
    },
    gen_server:cast(?MODULE, {send, Message}).

start_collector(PubId) ->
    publisher_supersup:start_collector(PubId).


handle_call(_, _, _State) ->
    {noreply, _State}.

handle_cast({send, Message}, State) ->
    #{socket := Socket} = State,

    Packet = serialize(Message),
    gen_tcp:send(Socket, Packet),
    % inet:setopts(Socket, [{active, once}]),

    {noreply, State}.

handle_info({_, _, Packet}, _State) ->
    Message = deserialize(Packet),
    io:format("Publisher Received: ~p~n", [Message]),
    
    {noreply, _State};

handle_info({tcp_closed, _}, _State) ->
    io:format("Client: ~p~n", [tcp_closed]),
    {stop, normal, _State}.


serialize(Message) ->
    jsx:encode(Message).

deserialize(Packet) ->
    jsx:decode(list_to_binary(Packet)).
