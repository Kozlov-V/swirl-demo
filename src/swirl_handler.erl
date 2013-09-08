-module(swirl_handler).

-export([
    init/4,
    stream/3,
    info/3,
    terminate/2
]).

-record(state, {
    mapper_nodes,
    reducer_node,
    flow_id
}).

init(_Transport, Req, _Opts, _Active) ->
    MapperNodes = [node()],
    ReducerNode = node(),

    FlowId = swirl_flow:start(swirl_demo_flow, [
        {stream_name, video},
        {stream_filter, "exchange_id = 3"},
        {reducer_opts, [
          {send_to, self()}
        ]}
    ], MapperNodes, ReducerNode),

	{ok, Req, #state {
        mapper_nodes = MapperNodes,
        reducer_node = ReducerNode,
        flow_id = FlowId
    }}.

stream(<<"ping">>, Req, State) ->
	{ok, Req, State};
stream(_Data, Req, State) ->
	{ok, Req, State}.

info({flow, _Period, Counters}, Req, State) ->
    Counters2 = jiffy:encode({[
        {<<"counters">>, {map_counters_json(Counters)}}
    ]}),
	{reply, Counters2, Req, State};
info(_Info, Req, State) ->
	{ok, Req, State}.

terminate(_Req, #state {
        mapper_nodes = MapperNodes,
        reducer_node = ReducerNode,
        flow_id = FlowId
    }) ->

    swirl_flow:stop(FlowId, MapperNodes, ReducerNode),
	ok.

%% private
map_counters_json([]) ->
    [];
map_counters_json([{Key, Value} | T]) ->
    [to_json(Key, Value) | map_counters_json(T)].

to_json({Type}, Value) ->
    {atom_to_binary(Type, utf8), Value}.
