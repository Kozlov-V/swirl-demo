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
    ok = application:start(crypto),
    ok = application:start(ranch),
    ok = application:start(cowboy),
    ok = application:start(swirl),
    ok = application:start(swirl_demo).

%% application callbacks
start(_StartType, _StartArgs) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/assets/[...]", cowboy_static, [
                {directory, {priv_dir, swirl_demo, [<<"assets">>]}},
                {mimetypes, swirl_demo_utils:mimetypes()}
            ]},
            {"/bullet/[...]", cowboy_static, [
                {directory, {priv_dir, bullet, []}},
                {mimetypes, swirl_demo_utils:mimetypes()}
            ]},
            {"/swirl", bullet_handler, [
                {handler, swirl_handler}
            ]},
            {'_', toppage_handler, []}
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
    io:format("To generate events enter: swirl_demo_utils:emit().~n", []),

    swirl_demo_sup:start_link().

stop(_State) ->
    ok.
