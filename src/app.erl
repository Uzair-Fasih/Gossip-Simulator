-module(app).
-import(full_network, [generateGrid/4]).
-import(linear_topology, [generateLinearGrid/4]).
-import('2d_grid', [generate2dGrid/3]).
-import(gossip, [getInitialState/1, updateState/1, shouldTerminate/1]).
-export([start/3]).

getAlgorithmParams(Algorithm) ->
  case(Algorithm) of
    (gossip_algo) -> State = gossip:getInitialState({10}),
      UpdateState = fun gossip:updateState/1,
      TerminateState = fun gossip:shouldTerminate/1,
      {State, UpdateState, TerminateState};
    (push_sum) -> pass
  end.
  

generateGrid(ServerPID, Topology, Algorithm, NodeCount, RumourCount) ->
  % {ok, RumourCount} = application:get_env(gossip, rumourCount),
  AlgorithmParams = getAlgorithmParams(Algorithm),
  case(Topology) of
    (full_network_topology) ->
      full_network:generateGrid(ServerPID, AlgorithmParams, NodeCount);
    (linear_topology) -> 
      linear_topology:generateLinearGrid(ServerPID, AlgorithmParams, NodeCount, true);
    ('2d_grid') -> 
      '2d_grid':generate2dGrid(ServerPID, AlgorithmParams, NodeCount)
    end.

monitorMetric(NodeCount, Count, Metrics) ->
  if Count == NodeCount -> done;
  true ->
    receive
      {record_metric} ->
        {Time, _} = statistics(wall_clock),
        ElapsedTime = Time * 1000,
        io:format("Nodes covered: ~p, Time Elaspsed: ~p~n", [Count + 1, ElapsedTime]),
        monitorMetric(NodeCount, Count + 1, lists:append(Metrics, [{ Count + 1, ElapsedTime }]))
    end
  end.


start(NodeCount, Topology, Algorithm) ->
  % {ok, W} = application:get_env(gossip, w), % Will be used only in the push-sum algorithm
  io:format("Running gossip simulator ~n"),
  io:format("Topology: ~s~n", [Topology]),
  io:format("Algorithm: ~s~n", [Algorithm]),
  io:format("NodeCount: ~p~n", [NodeCount]),
  % case(Topology) of 
  %   ('2d_topology') -> if 
  generateGrid(self(), Topology, Algorithm, NodeCount, 10),
  statistics(wall_clock),
  monitorMetric(NodeCount, 0, []).