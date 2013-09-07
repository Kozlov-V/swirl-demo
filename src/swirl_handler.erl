-module(swirl_handler).

-export([
    init/4,
    stream/3,
    info/3,
    terminate/2
]).

-record(state, {}).

init(_Transport, Req, _Opts, _Active) ->
    _FlowId = swirl_flow:start(swirl_demo_flow, [
        {stream_name, video},
        {stream_filter, "exchange_id = 3"},
        {reducer_opts, [
          {send_to, self()}
        ]}
    ], [node()], node()),

	{ok, Req, #state {}}.

stream(<<"ping">>, Req, State) ->
	{ok, Req, State};
stream(Data, Req, State) ->
	io:format("stream received ~s~n", [Data]),
	{ok, Req, State}.

info({flow, _Period, Counters}, Req, State) ->
    Counters2 = jiffy:encode({[
        {<<"counters">>, {map_counters_json(Counters)}}
    ]}),
	{reply, Counters2, Req, State};
info(Info, Req, State) ->
	io:format("info received ~p~n", [Info]),
	{ok, Req, State}.

terminate(_Req, _state) ->
	io:format("bullet terminate~n"),
	ok.

%% private
map_counters_json([]) ->
    [];
map_counters_json([{Key, Value} | T]) ->
    [to_json(Key, Value) | map_counters_json(T)].

to_json({Type}, Value) ->
    {atom_to_binary(Type, utf8), Value}.
