-module(linear_topology).
-export([generateGrid/4]).
-export([getNeighbors/2]).

getNeighbors(NodeIdx, Nodes) ->
  {_, Neighbors} = lists:unzip(lists:filter(fun ({CurrentIdx, _ }) -> (NodeIdx =/= CurrentIdx) and (abs(NodeIdx - CurrentIdx) =< 1) end, Nodes)),
  Neighbors.

% In linear topology, every node can communicate with its adjacent nodes
generateGrid(ServerPID, Algorithm, NodeCount, Config) ->
  {GetInitialState, UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState} = Algorithm,
  io:format("Generating a linear topology of nodes: ~p~n", [NodeCount]),
  Nodes = [
    {I, spawn_link(node, initialize, [ServerPID, {GetInitialState(Config, I), UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState}])} ||
    I <- lists:seq(1, NodeCount)
  ],

  lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, Nodes)} end, Nodes),
  {_, RandomNode} = lists:nth(rand:uniform(length(Nodes)), Nodes),
  RandomNode ! {receive_rumour, GetRumourData(pass)},

  length(Nodes).
