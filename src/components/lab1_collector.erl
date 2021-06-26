-module(lab1_collector).

-export([start/1, stop/1]).

start(Stream) ->
    {ok, Conn} = shotgun:open("localhost", 4000),
    Options = #{
        async => true, 
        async_mode => sse,
        handle_event => fun (_, _, Tweet) -> lab1_router_spec:route(Tweet) end
    },
    {ok, _Ref} = shotgun:get(Conn, Stream, #{}, Options),
    wait(150),
    shotgun:close(Conn),
    {ok, self()}.

wait(MSec) -> 
    receive  
    after 
        MSec -> ok 
    end.

stop(_State) -> ok.