-module(lab3_pubdb).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_call/3, handle_cast/2, handle_info/2]).
-export([connect/1, disconnect/1]).

-define(PUBTABLE, pubtable).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    io:format("~p: ~p~n", ["PubDB", self()]),

    ets:new(?PUBTABLE, [set, named_table]),
    
    {ok, {}}.

connect(ConnectInfo) ->
    gen_server:call(?MODULE, {connect, ConnectInfo}).

disconnect(DisconnectInfo) ->
    gen_server:call(?MODULE, {disconnect, DisconnectInfo}).


handle_call({connect, ConnectInfo}, _, _State) ->
    {PubId, Port, Topics} = ConnectInfo,

    ets:insert(?PUBTABLE, {PubId, Port, Topics}),
    lab3_tq_sup:ensure_tqs(Topics),

    {reply, "Connected", _State};

handle_call({disconnect, DisconnectInfo}, _, _State) ->
    {PubId} = DisconnectInfo,

    ets:delete(?PUBTABLE, PubId),

    {reply, "Disconnected", _State}.

handle_cast(_, State) ->
    {noreply, State}.

handle_info(status, _State) ->
    io:format("PubDB: ~p~n", [ets:tab2list(?PUBTABLE)]),
    {noreply, _State};

handle_info(_, _State) ->
    io:format("PubDB: what?~n", []),
    {noreply, _State}.