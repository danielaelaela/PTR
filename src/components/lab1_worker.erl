-module(lab1_worker).
-behaviour(gen_server).

-export([start_link/0, init/1, handle_cast/2]).

start_link() -> 
    gen_server:start_link(?MODULE, [], []).

init([]) -> 
    io:format("~p: ~p~n", ["Worker", self()]),
    {ok, {}}.

handle_cast(Event, _State) ->
    sleep(),
    EventMap = shotgun:parse_event(Event),
    #{data := Data} = EventMap,
    IsJson = jsx:is_json(Data),

    Text = process(Data, IsJson),
    Score = get_score(Text),
    io:format("~p: ~p~n", [self(), Score]),
    {noreply, _State}.


process(Data, IsJson) when IsJson == true ->
    Json = jsx:decode(Data),
    #{<<"message">> := #{<<"tweet">> := #{<<"text">> := Text}}} = Json,
    unicode:characters_to_list(Text, utf8);

process(Data, IsJson) when IsJson == false ->
    Text = unicode:characters_to_list(Data, utf8),
    Index = string:str(Text, "panic"),
    if 
        Index > 0 ->
            io:format("~p: ~p~n", [self(), "Panic!"]),
            exit(normal);
        Index =< 0 -> 
            "ok"
    end.

get_score(Text) ->
    LowText = string:lowercase(Text),
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
