-module('imperfect_3d_grid').
-export([generateGrid/4]).

% In full network, every node can communicate with every other node
generateGrid(ServerPID, Algorithm, NodeCount, Config) ->
  Rows = trunc(math:floor(math:sqrt(NodeCount))),
  Cols = Rows + (NodeCount rem Rows),
  {GetInitialState, UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState} = Algorithm,

  io:format("Generating a 3d grid of rows: ~p and columns: ~p~n", [Rows, Cols]),
  
  Nodes = [{X, Y, spawn_link(node, initialize, [ServerPID, {GetInitialState(Config, X + Y), UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState}])} || X <- lists:seq(1, Rows), Y <- lists:seq(1, Cols)],
  
  lists:foreach(
    fun({X, Y, NodePID}) -> 
      Neighbors = lists:foldl(
        fun({NX, NY, NeighborPID}, Neighbors) -> 
          if X + 1 == NX; X - 1 == NX; Y + 1 == NY; Y - 1 == NY ->
            [NeighborPID | Neighbors];
            true -> Neighbors
          end 
        end, [], Nodes
      ),

      % TODO: Filter added nodes from Nodes below
      { _, _, RandomNode} = lists:nth(rand:uniform(length(Nodes)), Nodes),

      NodePID ! { register_neighbours, [RandomNode | Neighbors] }
    end, 
    Nodes
  ),

  { _, _, RandomNode} = lists:nth(rand:uniform(length(Nodes)), Nodes),
  RandomNode ! {receive_rumour, GetRumourData(pass)},
  Nodes.