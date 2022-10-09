-module(node).
-export([initialize/2]).

% Each node on the grid must have the following properties
% It should maintain it's own state information
% State information includes knowledge about its neighbors
% It should have a controller and following methods: Send and Receive
% On receive the actor should decide whether it is to terminate

loop(ServerPID, Algorithm, Neighbors, Init) ->
  {State, UpdateState, ShouldTerminate} = Algorithm,
  if length(Neighbors) > 0 ->
    lists:nth(rand:uniform(length(Neighbors)), Neighbors) ! {receive_rumour};
    true -> pass
  end,

  TerminationCondition = ShouldTerminate(State),
  if TerminationCondition; length(Neighbors) == 0 -> % TerminationCondition mist be update
    io:fwrite("DEATH of ~p~n", [self()]),
    [Pid ! {announce_death, self()} || Pid <- Neighbors];
    true ->
      receive
        {receive_rumour} ->
          if Init == true ->
            ServerPID ! {record_metric};
            true -> pass
          end,

          % Send to a random neighbour
          % io:format("~p Received Rumour. Current count: ~p~n", [self(), CurrCount + 1]),
          loop(ServerPID, {UpdateState(State), UpdateState, ShouldTerminate}, Neighbors, false); % State needs to be updated

        {announce_death, Pid} -> 
          RemainingNeighbors = lists:filter(fun (Elem) -> Elem =/= Pid end, Neighbors),
          % io:format("Number of neighbors alive ~p~n", [length(RemainingNeighbors)]),
          loop(ServerPID, Algorithm, RemainingNeighbors, Init)
      end
  end.

initialize(ServerPID, Algorithm) ->
  receive 
    {register_neighbours, Neighbors, Node} ->
      io:fwrite("Received neighbors from ~p: ~p~n" , [Node, Neighbors]),
      loop(ServerPID, Algorithm, Neighbors, true) % The inital state must be passed by algorithm. State = {RumourCount, 0}
  end.