-module(full_network).
-export([generateGrid/4]).

% In full network, every node can communicate with every other node
generateGrid(ServerPID, Algorithm, AlgorithmParams, NodeCount) ->
  Nodes = [
    {S, spawn_link(node, initialize, [ServerPID, Algorithm, AlgorithmParams])} ||
    S <- lists:seq(1, NodeCount)
  ],
  {_, DenumeratedNodes} = lists:unzip(Nodes),
  case(Algorithm) of
    ('gossip_algo') -> lists:foreach(fun({_, NodePID}) -> NodePID ! {register_neighbours, lists:filter(fun (Elem) -> Elem =/= NodePID end, DenumeratedNodes) , NodePID} end, Nodes),
      lists:nth(rand:uniform(length(Nodes)), DenumeratedNodes) ! {receive_rumour};
    ('push_sum') -> lists:foreach(fun({Idx, NodePID}) -> NodePID ! {register_neighbours, lists:filter(fun (Elem) -> Elem =/= NodePID end, DenumeratedNodes), NodePID, Idx} end, Nodes),
      lists:nth(rand:uniform(length(Nodes)), DenumeratedNodes) ! {receive_sum, {rand:uniform(length(Nodes)), 1}}

    end,
  
  Nodes.
