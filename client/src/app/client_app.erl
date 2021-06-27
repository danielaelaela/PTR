-module(client_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    client_supersup:start_link().

stop(_State) ->
    ok.
