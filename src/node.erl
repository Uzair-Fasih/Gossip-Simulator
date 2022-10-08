-module(node).
-export([initialize/2]).
% Each node on the grid must have the following properties
% It should maintain it's own state information
% State information includes knowledge about its neighbors
% It should have a controller and following methods: Send and Receive
% On receive the actor should decide whether it is to termintae

loop(ServerPID, RumourCount, CurrCount, Neighbors) ->
  if CurrCount == 1 ->
    ServerPID ! {record_metric};
    true -> pass
  end,

  if length(Neighbors) > 0 ->
    lists:nth(rand:uniform(length(Neighbors)), Neighbors) ! {receive_rumour};
    true -> pass
  end,

  if CurrCount > RumourCount; length(Neighbors) == 0 -> 
    [Pid ! {announce_death, self()} || Pid <- Neighbors];
    true ->
      receive
        {receive_rumour} ->
          % Send to a random neighbour
          % io:format("~p Received Rumour. Current count: ~p~n", [self(), CurrCount + 1]),
          loop(ServerPID, RumourCount, CurrCount + 1, Neighbors);

        {announce_death, Pid} -> 
          RemainingNeighbors = lists:filter(fun (Elem) -> Elem =/= Pid end, Neighbors),
          % io:format("Number of neighbors alive ~p~n", [length(RemainingNeighbors)]),
          loop(ServerPID, RumourCount, CurrCount, RemainingNeighbors)
      end
  end.

initialize(ServerPID, RumourCount) ->
  receive 
    {register_neighbours, Neighbors} -> 
      loop(ServerPID, RumourCount, 0, Neighbors)
  end.