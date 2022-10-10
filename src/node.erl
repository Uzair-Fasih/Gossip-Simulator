-module(node).
-export([initialize/2]).

loop(ServerPID, Algorithm, Neighbors, Init) ->
  {State, UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState} = Algorithm,

  % If there are neigbors for a node to communicate with, send rumor to a random neighbor
  SettleState = if length(Neighbors) > 0, Init =/= true ->
    lists:nth(rand:uniform(length(Neighbors)), Neighbors) ! {receive_rumour, GetRumourData(State)},
    GetStateData(State);
    true -> pass
  end,

  NewState = GetSettledState(State, SettleState),

  TerminationCondition = ShouldTerminate(NewState),
  if TerminationCondition; length(Neighbors) == 0 ->
    [Pid ! {announce_death, self()} || Pid <- Neighbors];
    true -> 
      receive
        {receive_rumour, ReceivedData} ->
          if Init == true ->
            ServerPID ! {record_metric};
            true -> pass
          end,

          loop(ServerPID, {UpdateState(NewState, ReceivedData), UpdateState, ShouldTerminate, GetRumourData, GetStateData, GetSettledState}, Neighbors, false);

        {announce_death, Pid} -> 
          RemainingNeighbors = lists:filter(fun (Elem) -> Elem =/= Pid end, Neighbors),
          loop(ServerPID, Algorithm, RemainingNeighbors, Init)
      end
  end.

initialize(ServerPID, Algorithm) ->
  receive 
    {register_neighbours, Neighbors} -> 
      loop(ServerPID, Algorithm, Neighbors, true)
  end.