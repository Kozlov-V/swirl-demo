-module(swirl_demo).

%% public
-export([
    emit/0,
    emit/1,
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
    emit(?N).

emit(N) ->
    random:seed(erlang:now()),
    Timestamp = os:timestamp(),
    emit_loop(N),
    Delta = timer:now_diff(os:timestamp(), Timestamp),
    io:format("Average stream:emit/2 time: ~p microseconds~n", [Delta / N]).

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

    io:format("~n", []),
    io:format("                         __            __                __                                   ~n", []),
    io:format("                        |  \\          |  \\              |  \\                                  ~n", []),
    io:format("  _______  __   __   __  \\$$  ______  | $$          ____| $$  ______   ______ ____    ______  ~n", []),
    io:format(" /       \\|  \\ |  \\ |  \\|  \\ /      \\ | $$ ______  /      $$ /      \\ |      \\    \\  /      \\ ~n", []),
    io:format("|  $$$$$$$| $$ | $$ | $$| $$|  $$$$$$\\| $$|      \\|  $$$$$$$|  $$$$$$\\| $$$$$$\\$$$$\\|  $$$$$$\\~n", []),
    io:format(" \\$$    \\ | $$ | $$ | $$| $$| $$   \\$$| $$ \\$$$$$$| $$  | $$| $$    $$| $$ | $$ | $$| $$  | $$~n", []),
    io:format(" _\\$$$$$$\\| $$_/ $$_/ $$| $$| $$      | $$        | $$__| $$| $$$$$$$$| $$ | $$ | $$| $$__/ $$~n", []),
    io:format("|       $$ \\$$   $$   $$| $$| $$      | $$         \\$$    $$ \\$$     \\| $$ | $$ | $$ \\$$    $$~n", []),
    io:format(" \\$$$$$$$   \\$$$$$\\$$$$  \\$$ \\$$       \\$$          \\$$$$$$$  \\$$$$$$$ \\$$  \\$$  \\$$  \\$$$$$$~n", []),
    io:format("~n", []),
    io:format("Video dashboard is running on: http://localhost:8080/~n", []),
    io:format("To generate events enter: swirl_demo:emit().~n", []),

    swirl_demo_sup:start_link().

stop(_State) ->
    ok.

%% private
emit_loop(0) ->
    ok;
emit_loop(N) ->
    swirl_stream:emit(video, random_event()),
    emit_loop(N-1).

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
