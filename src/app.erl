-module(app).
-export([start/3]).

getAlgorithmParams(Algorithm, RumourCount) ->
  {AlgorithmParams, Config} = case(Algorithm) of
    (gossip_algo) -> {{ 
      fun gossip:getInitialState/2,
      fun gossip:updateState/2, 
      fun gossip:shouldTerminate/1, 
      fun gossip:getRumourData/1,
      fun gossip:getStateData/1,
      fun gossip:getSettledState/2
    }, { RumourCount }};

    (push_sum_algo) -> {{ 
      fun push_sum:getInitialState/2,
      fun push_sum:updateState/2, 
      fun push_sum:shouldTerminate/1, 
      fun push_sum:getRumourData/1,
      fun push_sum:getStateData/1,
      fun push_sum:getSettledState/2
    }, pass}
    end,
  {AlgorithmParams, Config}.

generateGrid(ServerPID, Topology, Algorithm, NodeCount, RumourCount) ->
  {AlgorithmParams, Config} = getAlgorithmParams(Algorithm, RumourCount),
  case(Topology) of
    (full_network_topology) ->
      full_network:generateGrid(ServerPID, AlgorithmParams, NodeCount, Config);
    (linear_topology) -> 
      linear_topology:generateGrid(ServerPID, AlgorithmParams, NodeCount, Config);
    ('2d_topology') -> 
      '2d_topology':generateGrid(ServerPID, AlgorithmParams, NodeCount, Config);
    (imperfect_3d_grid) ->
      imperfect_3d_grid:generateGrid(ServerPID, AlgorithmParams, NodeCount, Config)
    end.

monitorMetric(NodeCount, Count, Metrics) ->
  if Count == NodeCount -> 'The Topology has converged';
  true ->
    receive
      {record_metric} ->
        {_ , Time} = statistics(wall_clock),
        ElapsedTime = Time * 1000,
        io:format("Nodes covered: ~p, Time Elaspsed: ~p~n", [Count + 1, ElapsedTime]),
        monitorMetric(NodeCount, Count + 1, lists:append(Metrics, [{ Count + 1, ElapsedTime }]))
    end
  end.

start(NodeCount, Topology, Algorithm) ->
  {ok, RumourCount} = application:get_env(gossip, rumourCount), % Will be used only in the push-sum algorithm
  io:format("Running gossip simulator ~n"),
  io:format("Topoplogy: ~s~n", [Topology]),
  io:format("Algorithm: ~s~n", [Algorithm]),
  io:format("NodeCount: ~p~n", [NodeCount]),
  statistics(wall_clock),
  generateGrid(self(), Topology, Algorithm, NodeCount, RumourCount),
  monitorMetric(NodeCount, 0, []).