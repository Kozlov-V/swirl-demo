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
    Headers = [
        {<<"Content-Type">>, <<"text/html">>}
    ],
    {ok, Template} = video_dashboard_dtl:render(),
    {ok, Req2} = cowboy_req:reply(200, Headers, Template, Req),
    {ok, Req2, State}.

terminate(_Reason, _Req, _State) ->
    ok.
