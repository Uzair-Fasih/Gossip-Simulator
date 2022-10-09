-module('2d_grid').
-export([generate2dGrid/3]).

enumerate(List) ->
  {List1, _ } = lists:mapfoldl(fun(T, Acc) -> {{Acc, T}, Acc + 1} end, 0, List),
  List1.

generateNodes(ServerPID, Algorithm, NodeCount) ->
  Nodes = [
    spawn_link(node, initialize, [ServerPID, Algorithm]) ||
    _ <- lists:seq(1, NodeCount)
  ],
  NewNodes = enumerate(Nodes),
  NewNodes.

checkAdjacency(NodeIdx, CurrentIdx, RowCount) ->
    if trunc(NodeIdx / RowCount) == trunc(CurrentIdx / RowCount) -> true;
    true -> false
    end.

checkIndices(NodeIdx, CurrentIdx, RowCount) ->
    ((abs(NodeIdx - CurrentIdx) =< 1) and checkAdjacency(NodeIdx, CurrentIdx, RowCount)) or (CurrentIdx - RowCount == NodeIdx) or ((CurrentIdx + RowCount == NodeIdx)).

getNeighbors(NodeIdx, RowCount, Nodes) ->
    {_, Neighbors} = lists:unzip(lists:filter(fun ({CurrentIdx, _ }) -> (NodeIdx =/= CurrentIdx) and checkIndices(NodeIdx, CurrentIdx, RowCount)  end, Nodes)),
    Neighbors.

generateGrid(RowCount, Nodes) ->
    lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, RowCount, Nodes), NodePID} end, Nodes).

% In 2d network, every node can communicate with adjacent nodes
generate2dGrid(ServerPID, Algorithm, NodeCount) ->
  RowCount = trunc(math:sqrt(NodeCount)),
  Nodes = generateNodes(ServerPID, Algorithm, NodeCount),
  io:fwrite("NodeRow : ~w~n", [Nodes]),
  generateGrid(RowCount, Nodes),
  {_, NodeList} = lists:unzip(Nodes),
  lists:nth(rand:uniform(NodeCount), NodeList) ! {receive_rumour}.
