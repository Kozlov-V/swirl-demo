-module(swirl_demo).

%% public
-export([
    emit/0,
    start/0
]).

-behaviour(application).
-export([
    start/2,
    stop/1
]).

-define(N, 10000000).

%% public
emit() ->
    random:seed(erlang:now()),
    Timestamp = os:timestamp(),
    emit(?N),
    Delta = timer:now_diff(os:timestamp(), Timestamp),
    io:format("average stream emit time: ~p microseconds~n", [Delta / ?N]).

start() ->
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(swirl),
    ok = application:start(swirl_demo).

%% application callbacks
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", toppage_handler, []},
            {"/assets/[...]", cowboy_static, [
                {directory, {priv_dir, swirl_demo, [<<"assets">>]}},
                {mimetypes, mimetypes()}
            ]},
            {"/bullet/[...]", cowboy_static, [
                {directory, {priv_dir, bullet, []}},
                {mimetypes, mimetypes()}
            ]},
            {"/swirl", bullet_handler, [
                {handler, swirl_handler}
            ]}
        ]}
    ]),

    TransOpts = [{port, 8080}],
    ProtoOpts = [{env, [
        {dispatch, Dispatch}
    ]}],
    {ok, _} = cowboy:start_http(http, 8, TransOpts, ProtoOpts),

    io:format("~nswirl-demo: http://localhost:8080/~n", []),
    swirl_demo_sup:start_link().

stop(_State) ->
    ok.

%% private
emit(0) ->
    ok;
emit(N) ->
    swirl_stream:emit(video, random_event()),
    emit(N-1).

random_event() ->
    lists:nth(random:uniform(14), [
        [{type, start}, {exchange_id, 3}],
        [{type, start}, {exchange_id, 3}],
        [{type, start}, {exchange_id, 3}],
        [{type, start}, {exchange_id, 3}],
        [{type, start}, {exchange_id, 3}],
        [{type, start}, {exchange_id, 3}],
        [{type, midpoint}, {exchange_id, 3}],
        [{type, midpoint}, {exchange_id, 3}],
        [{type, midpoint}, {exchange_id, 3}],
        [{type, complete}, {exchange_id, 3}],
        [{type, complete}, {exchange_id, 3}],
        [{type, pause}, {exchange_id, 3}],
        [{type, resume}, {exchange_id, 3}],
        [{type, rewind}, {exchange_id, 3}]
    ]).

mimetypes() ->
    [{<<".css">>, [<<"text/css">>]},
     {<<".js">>, [<<"application/javascript">>]},
     {<<".png">>, [<<"image/png">>]}].
