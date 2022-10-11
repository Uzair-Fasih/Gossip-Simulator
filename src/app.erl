-module(app).
-export([start/3, performanceMonitor/3, aggregateMetrics/2]).

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

aggregateMetrics(Count, ElaspedTime) -> 
  receive
    {record_metric} ->
      {Time, _} = statistics(wall_clock),
      aggregateMetrics(Count + 1, Time);
    {request_metric, ProcessPID} ->
      ProcessPID ! {metric, Count, ElaspedTime},
      aggregateMetrics(Count, ElaspedTime);
    {shutdown} -> done
  end.

% Performance Monitor prints metrics to the screen after every 10s
performanceMonitor(ServerPID, PerformanceInterval, NodeCount) ->
  timer:sleep(PerformanceInterval),
  ServerPID ! {request_metric, self()},
  receive
    {metric, Count, Time} ->
      io:format("Nodes Covered: ~p, Time Elapsed: ~p~n", [Count, Time * 1000]),
      if Count == NodeCount -> ServerPID ! {shutdown}, io:format("Done~n");
        true -> performanceMonitor(ServerPID, PerformanceInterval, NodeCount)
      end
  end.

start(NodeCount, Topology, Algorithm) ->
  % {ok, RumourCount} = application:get_env(gossip, rumourCount), % Will be used only in the push-sum algorithm
  io:format("Running gossip simulator ~n"),
  io:format("Topoplogy: ~s~n", [Topology]),
  io:format("Algorithm: ~s~n", [Algorithm]),
  io:format("NodeCount: ~p~n~n", [NodeCount]),
  SupervisorID = spawn(?MODULE, aggregateMetrics, [0, 0]),
  ImplementedNodeCount = generateGrid(SupervisorID, Topology, Algorithm, NodeCount, 10),
  spawn(?MODULE, performanceMonitor, [SupervisorID, 500, ImplementedNodeCount]),
  io:format("Nodes Covered: ~p, Time Elapsed: ~p~n", [0, 0]).
