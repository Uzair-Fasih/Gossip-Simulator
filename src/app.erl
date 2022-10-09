-module(app).
-import(gossip, [getInitialState/1, updateState/1, shouldTerminate/1]).
-export([start/3]).

% Full Network Topology
generateGrid(ServerPID, full_network_topology, gossip_algo, NodeCount, RumourCount) ->
  % {ok, RumourCount} = application:get_env(gossip, rumourCount),
  State = gossip:getInitialState({10}),
  full_network:generateGrid(
    ServerPID, 
    { State, fun gossip:updateState/1, fun gossip:shouldTerminate/1 },
    NodeCount
  );

% Imperfect 3D Grid Topology
generateGrid(ServerPID, imperfect_3d_grid, gossip_algo, NodeCount, RumourCount) ->
  % {ok, RumourCount} = application:get_env(gossip, rumourCount),
  State = gossip:getInitialState({10}),
  imperfect_3d_grid:generateGrid(
    ServerPID, 
    { State, fun gossip:updateState/1, fun gossip:shouldTerminate/1 },
    NodeCount
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
  generateGrid(self(), Topoplogy, Algorithm, NodeCount, 10),
  statistics(wall_clock),
  monitorMetric(NodeCount, 0, []).