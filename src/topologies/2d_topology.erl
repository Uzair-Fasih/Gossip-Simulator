-module('2d_topology').
-export([generateGrid/4]).

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

registerNeighbors(RowCount, Nodes) ->
    lists:foreach(fun({NodeIdx, NodePID}) -> NodePID ! {register_neighbours, getNeighbors(NodeIdx, RowCount, Nodes)} end, Nodes),
    {_, RandomNode} = lists:nth(rand:uniform(length(Nodes)), Nodes),
    fun(Y) -> RandomNode ! {receive_rumour, Y(pass)} end.


generateNodes(ServerPID, Algorithm, NodeCount, Config) ->
   {GetInitialState, UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState} = Algorithm,
  Nodes = [
    {I, spawn_link(node, initialize, [ServerPID, {GetInitialState(Config, I), UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState}])} ||
    I <- lists:seq(1, NodeCount)
  ],
  {Nodes, GetRumourData}.

% In 2d topology, every node can communicate with adjacent nodes in the same row and column
generateGrid(ServerPID, Algorithm, NodeCount, Config) ->
  RowCount = trunc(math:sqrt(NodeCount)),
  io:format("Generating a perfect 2d grid of rows: ~p~n", [RowCount]),
  {Nodes, GetRumourData} = generateNodes(ServerPID, Algorithm, NodeCount, Config),
  SendRumor = registerNeighbors(RowCount, Nodes),
  SendRumor(GetRumourData),
  Nodes.
