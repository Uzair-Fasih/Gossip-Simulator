-module('2d_grid').
-export([generate2dGrid/4]).

generateNodes(ServerPID, Algorithm, AlgorithmParams, NodeCount) ->
  Nodes = [
    {S, spawn_link(node, initialize, [ServerPID, Algorithm, AlgorithmParams])} ||
    S <- lists:seq(1, NodeCount)
  ],
  Nodes.

checkAdjacency(NodeIdx, CurrentIdx, RowCount) ->
    A = math:ceil(NodeIdx / RowCount),
    B = math:ceil(CurrentIdx / RowCount),
    if A == B -> true;
    true -> false
    end.

checkIndices(NodeIdx, CurrentIdx, RowCount) ->
    ((abs(NodeIdx - CurrentIdx) =< 1) and checkAdjacency(NodeIdx, CurrentIdx, RowCount)) or (CurrentIdx - RowCount == NodeIdx) or ((CurrentIdx + RowCount == NodeIdx)).

getNeighbors(NodeIdx, RowCount, Nodes) ->
    {_, Neighbors} = lists:unzip(lists:filter(fun ({CurrentIdx, _ }) -> (NodeIdx =/= CurrentIdx) and checkIndices(NodeIdx, CurrentIdx, RowCount)  end, Nodes)),
    Neighbors.

getNthPid(Nodes) ->
    {_, [PID | _ ]} = lists:unzip([lists:nth(rand:uniform(length(Nodes)), Nodes)]),
    PID.

registerNeighbors(RowCount, Algorithm, Nodes) ->
    case(Algorithm) of
    ('gossip_algo') -> lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, RowCount, Nodes), NodePID} end, Nodes),
      getNthPid(Nodes) ! {receive_rumour};
    ('push_sum') -> lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, RowCount, Nodes), NodePID, NodeIdx} end, Nodes),
      getNthPid(Nodes) ! {receive_sum,  {rand:uniform(length(Nodes)), 1}}
    end.

% In 2d topology, every node can communicate with adjacent nodes in the same row and column
generate2dGrid(ServerPID, Algorithm, AlgorithmParams, NodeCount) ->
  RowCount = trunc(math:sqrt(NodeCount)),
  Nodes = generateNodes(ServerPID, Algorithm, AlgorithmParams, NodeCount),
  io:fwrite("NodeRow : ~w~n", [Nodes]),
  registerNeighbors(RowCount, Algorithm, Nodes).
