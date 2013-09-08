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
    Headers = [{<<"Content-Type">>, <<"text/html">>}],
    {Querystring, Req2} = cowboy_req:qs(Req),
    {ok, Template} = video_dashboard_dtl:render([{querystring, Querystring}]),
    {ok, Req3} = cowboy_req:reply(200, Headers, Template, Req2),
    {ok, Req3, State}.

terminate(_Reason, _Req, _State) ->
    ok.
