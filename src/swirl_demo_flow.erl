-module(swirl_demo_flow).

-behavior(swirl_flow).
-export([
    map/3,
    reduce/3
]).

%% swirl_flow callbacks
map(_StreamName, Event, _MapperOpts) ->
    Type = swirl_utils:lookup(type, Event),
    {update, {Type}, {1}}.

reduce(Period, Aggregates, ReducerOpts) ->
    Pid = swirl_utils:lookup(send_to, ReducerOpts),
    Pid ! {flow, Period, Aggregates}.
