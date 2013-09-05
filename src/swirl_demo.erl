-module(swirl_demo).

%% public
-export([
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

%% public
start() ->
    ok = application:start(swirl_demo).

%% application callbacks
start(_StartType, _StartArgs) ->
    swirl_demo_sup:start_link().

stop(_State) ->
    ok.
