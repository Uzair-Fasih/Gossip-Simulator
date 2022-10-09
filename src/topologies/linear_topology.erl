-module(linear_topology).
-export([generateLinearGrid/4]).
-export([getNeighbors/2]).

enumerate(List) ->
  {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc + 1} end, 1, List),
  List1.

getNeighbors(NodeIdx, Nodes) ->
  {_, Neighbors} = lists:unzip(lists:filter(fun ({CurrentIdx, _ }) -> (NodeIdx =/= CurrentIdx) and (abs(NodeIdx - CurrentIdx) =< 1) end, Nodes)),
  Neighbors.

% In linear topology, every node can communicate with its adjacent nodes
generateLinearGrid(ServerPID, Algorithm, AlgorithmParams, NodeCount) ->
  Nodes = [
    {S, spawn_link(node, initialize, [ServerPID, Algorithm, AlgorithmParams])} || 
    S <- lists:seq(1, NodeCount)
  ],
  % NewNodes = enumerate(Nodes),
  case(Algorithm) of
    ('gossip_algo') -> lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, Nodes), NodePID} end, Nodes),
      lists:nth(rand:uniform(length(Nodes)), Nodes) ! {receive_rumour};
    ('push_sum') -> lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, Nodes), NodePID, NodeIdx} end, Nodes),
      lists:nth(rand:uniform(length(Nodes)), Nodes) ! {receive_sum, {rand:uniform(length(Nodes)), 1}}

    end,
  Nodes.
