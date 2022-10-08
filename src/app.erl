-module(app).
-import(full_network, [generateGrid/4]).
-export([start/3]).

generateGrid(ServerPID, full_network_topology, Algorithm, NodeCount, RumourCount) ->
  full_network:generateGrid(ServerPID, Algorithm, NodeCount, RumourCount).

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
  % {ok, RumourCount} = application:get_env(gossip, rumourCount), % Will be used only in the Gossip algorithm
  io:format("Running gossip simulator ~n"),
  io:format("Topoplogy: ~s~n", [Topoplogy]),
  io:format("Algorithm: ~s~n", [Algorithm]),
  io:format("NodeCount: ~p~n", [NodeCount]),
  generateGrid(self(), Topoplogy, Algorithm, NodeCount, 10),
  statistics(wall_clock),
  monitorMetric(NodeCount, 0, []).