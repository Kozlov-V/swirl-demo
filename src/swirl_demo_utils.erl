-module(swirl_demo_utils).

-export([
    emit/0,
    emit/1,
    mimetypes/0,
    timestamp/0
]).

-define(N, 10000000).

%% public
emit() ->
    emit(?N).

emit(N) ->
    random:seed(erlang:now()),
    Timestamp = os:timestamp(),
    emit_loop(N),
    Delta = timer:now_diff(os:timestamp(), Timestamp),
    io:format("average time: ~p microseconds~n", [Delta / N]).

mimetypes() ->
    [{<<".css">>, [<<"text/css">>]},
     {<<".js">>, [<<"application/javascript">>]},
     {<<".png">>, [<<"image/png">>]}].

timestamp() ->
    {{YY, MM, DD}, {Hour, Min, Sec}} = calendar:now_to_local_time(os:timestamp()),
    io_lib:format("~4..0w-~2..0w-~2..0w ~2..0w:~2..0w:~2..0w",
        [YY, MM, DD, Hour, Min, Sec]).

%% private
emit_loop(0) ->
    ok;
emit_loop(N) ->
    swirl_stream:emit(video, random_event()),
    emit_loop(N-1).

random_event() ->
    lists:nth(random:uniform(14), [
        [{type, start}, {exchange_id, 1}],
        [{type, start}, {exchange_id, 2}],
        [{type, start}, {exchange_id, 3}],
        [{type, start}, {exchange_id, 4}],
        [{type, start}, {exchange_id, 5}],
        [{type, start}, {exchange_id, 1}],
        [{type, midpoint}, {exchange_id, 2}],
        [{type, midpoint}, {exchange_id, 3}],
        [{type, midpoint}, {exchange_id, 4}],
        [{type, complete}, {exchange_id, 5}],
        [{type, complete}, {exchange_id, 1}],
        [{type, pause}, {exchange_id, 2}],
        [{type, resume}, {exchange_id, 3}],
        [{type, rewind}, {exchange_id, 4}]
    ]).
