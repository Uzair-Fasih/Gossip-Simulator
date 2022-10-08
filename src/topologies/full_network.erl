-module(full_network).
-export([generateGrid/4]).

% In full network, every node can communicate with every other node
generateGrid(ServerPID, Algorithm, NodeCount, RumourCount) ->
  Nodes = [
    spawn_link(node, initialize, [ServerPID, RumourCount]) ||
    _ <- lists:seq(1, NodeCount)
  ],
  lists:foreach(fun(NodePID) -> NodePID ! {register_neighbours, lists:filter(fun (Elem) -> Elem =/= NodePID end, Nodes)} end, Nodes),
  lists:nth(rand:uniform(length(Nodes)), Nodes) ! {receive_rumour},
  Nodes.
  
