-module(linear_topology).
-export([generateLinearGrid/4]).
-export([getNeighbors/2]).

enumerate(List) ->
  {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc+1} end, 1, List),
  List1.

getNeighbors(NodeIdx, Nodes) ->
  {_, Neighbors} = lists:unzip(lists:filter(fun ({CurrentIdx, _ }) -> (NodeIdx =/= CurrentIdx) and (abs(NodeIdx - CurrentIdx) =< 1) end, Nodes)),
  Neighbors.

% In linear topology, every node can communicate with its adjacent nodes
generateLinearGrid(ServerPID, Algorithm, NodeCount, StartRumour) ->
  Nodes = [
    spawn_link(node, initialize, [ServerPID, Algorithm]) ||
    _ <- lists:seq(1, NodeCount)
  ],
  NewNodes = enumerate(Nodes),
  if StartRumour ->
    lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, NewNodes), NodePID} end, NewNodes),
    lists:nth(rand:uniform(length(Nodes)), Nodes) ! {receive_rumour};
    true -> pass
  end,
  Nodes.
