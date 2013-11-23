-module(toppage_handler).

-export([
    init/3,
    handle/2,
    terminate/3
]).

-record(state, {}).

%% public
init(_Transport, Req, []) ->
    {ok, Req, #state {}}.

handle(Req, State) ->
    {Method, Req2} = cowboy_req:method(Req),
    {RawPath, Req3} = cowboy_req:path(Req2),
    route(Method, RawPath, Req3, State).

terminate(_Reason, _Req, _State) ->
    ok.

%% private
route(<<"GET">>, <<"/">>, Req, State) ->
    Headers = [{<<"Content-Type">>, <<"text/html">>}],
    {Querystring, Req2} = cowboy_req:qs(Req),
    {ok, Template} = video_dashboard_dtl:render([{querystring, Querystring}]),
    {ok, Req3} = cowboy_req:reply(200, Headers, Template, Req2),
    {ok, Req3, State};
route(<<"GET">>, <<"/emit">>, Req, State) ->
    {Count, Req2} = cowboy_req:qs_val(<<"count">>, Req, <<"1">>),
    spawn(fun() -> swirl_demo_utils:emit(binary_to_integer(Count)) end),
    {ok, Req3} = cowboy_req:reply(200, [], <<>>, Req2),
    {ok, Req3, State};
route(_, _, Req, State) ->
    {ok, Req2} = cowboy_req:reply(404, [], <<>>, Req),
    {ok, Req2, State}.
