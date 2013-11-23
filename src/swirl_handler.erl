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
    {StreamFilter, Req2} = cowboy_req:qs_val(<<"filter">>, Req),
    MapperNodes = [node()],
    ReducerNode = node(),

    FlowId = swirl_flow:start(swirl_demo_flow, [
        {stream_name, video},
        {stream_filter, StreamFilter},
        {reducer_opts, [
          {send_to, self()}
        ]}
    ], MapperNodes, ReducerNode),

	{ok, Req2, #state {
        mapper_nodes = MapperNodes,
        reducer_node = ReducerNode,
        flow_id = FlowId
    }}.

stream(<<"ping">>, Req, State) ->
    {ok, Req, State};
stream(_Data, Req, State) ->
    {ok, Req, State}.

info({flow, _Period, Aggregates}, Req, State) ->
    Counters = jiffy:encode({[
        {<<"counters">>, {map_aggregates_json(Aggregates)}}
    ]}),
    % io:format("[~s] bullet: ~p~n", [swirl_demo_utils:timestamp(), Counters]),
    {reply, Counters, Req, State};
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
map_aggregates_json([]) ->
    [];
map_aggregates_json([{Key, Value} | T]) ->
    [to_json(Key, Value) | map_aggregates_json(T)].

to_json({Type}, Value) ->
    {atom_to_binary(Type, utf8), Value}.
