-module(lab1_app).
-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    lab1_supersup:start_link().

stop(_State) -> ok.
