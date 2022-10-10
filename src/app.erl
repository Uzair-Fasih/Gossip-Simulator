-module(app).
-export([start/3]).

% ---------------- Full Network Topology ---------------- 
generateGrid(ServerPID, full_network_topology, gossip_algo, NodeCount) ->
  % {ok, RumourCount} = application:get_env(gossip, rumourCount),
  full_network:generateGrid(
    ServerPID, 
    { 
      fun gossip:getInitialState/2,
      fun gossip:updateState/2, 
      fun gossip:shouldTerminate/1, 
      fun gossip:getRumourData/1,
      fun gossip:getStateData/1,
      fun gossip:getSettledState/2
    },
    NodeCount,
    { 10 }
  );

generateGrid(ServerPID, full_network_topology, push_sum_algo, NodeCount) ->
  % {ok, RumourCount} = application:get_env(gossip, rumourCount),
  full_network:generateGrid(
    ServerPID, 
    { 
      fun push_sum:getInitialState/2, 
      fun push_sum:updateState/2, 
      fun push_sum:shouldTerminate/1, 
      fun push_sum:getRumourData/1,
      fun push_sum:getStateData/1,
      fun push_sum:getSettledState/2
    },
    NodeCount,
    pass
  );

% ---------------- Imperfect 3D Grid Topology ---------------- 
generateGrid(ServerPID, imperfect_3d_grid, gossip_algo, NodeCount) ->
  % {ok, RumourCount} = application:get_env(gossip, rumourCount),
  imperfect_3d_grid:generateGrid(
    ServerPID, 
    { 
      fun gossip:getInitialState/2,
      fun gossip:updateState/2, 
      fun gossip:shouldTerminate/1, 
      fun gossip:getRumourData/1,
      fun gossip:getStateData/1,
      fun gossip:getSettledState/2
    },
    NodeCount,
    { 10 }
  );

generateGrid(ServerPID, imperfect_3d_grid, push_sum_algo, NodeCount) ->
  imperfect_3d_grid:generateGrid(
    ServerPID, 
    { 
      fun push_sum:getInitialState/2,
      fun push_sum:updateState/2, 
      fun push_sum:shouldTerminate/1, 
      fun push_sum:getRumourData/1,
      fun push_sum:getStateData/1,
      fun push_sum:getSettledState/2
    },
    NodeCount,
    pass
  ).

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


start(NodeCount, Topoplogy, Algorithm) ->
  % {ok, W} = application:get_env(gossip, w), % Will be used only in the push-sum algorithm
  io:format("Running gossip simulator ~n"),
  io:format("Topoplogy: ~s~n", [Topoplogy]),
  io:format("Algorithm: ~s~n", [Algorithm]),
  io:format("NodeCount: ~p~n", [NodeCount]),
  statistics(wall_clock),
  generateGrid(self(), Topoplogy, Algorithm, NodeCount),
  monitorMetric(NodeCount, 0, []).