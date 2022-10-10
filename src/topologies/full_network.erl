-module(full_network).
-export([generateGrid/4]).

% In a full network topoplogy, every node can communicate with every other node
generateGrid(ServerPID, Algorithm, NodeCount, Config) ->
  {GetInitialState, UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState} = Algorithm,

  Nodes = [
    spawn_link(node, initialize, [ServerPID, {GetInitialState(Config, I), UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState}]) ||
    I <- lists:seq(1, NodeCount)
  ],
  
  lists:foreach(fun(NodePID) -> NodePID ! {register_neighbours, lists:filter(fun (Elem) -> Elem =/= NodePID end, Nodes)} end, Nodes),
  
  RandomNode = lists:nth(rand:uniform(length(Nodes)), Nodes),
  RandomNode ! {receive_rumour, GetRumourData(pass)},
  Nodes.
