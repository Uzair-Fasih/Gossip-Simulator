-module(node).
-export([initialize/3]).

% Each node on the grid must have the following properties
% It should maintain it's own state information
% State information includes knowledge about its neighbors
% It should have a controller and following methods: Send and Receive
% On receive the actor should decide whether it is to terminate

loop(ServerPID, Algorithm, AlgorithmParams, Neighbors, Init) ->
  
  {State, UpdateState, ShouldTerminate} = AlgorithmParams,

  if length(Neighbors) > 0 ->
      lists:nth(rand:uniform(length(Neighbors)), Neighbors) ! {receive_rumour};
      true -> pass
    end,

  TerminationCondition = ShouldTerminate(State),
  % io:fwrite("~p TerminationCondition : ~p   ~p  ~n", [self(), TerminationCondition, length(Neighbors) == 0]),
  if TerminationCondition orelse (length(Neighbors) == 0) -> % TerminationCondition mist be update
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
          loop(ServerPID, Algorithm, {UpdateState(State), UpdateState, ShouldTerminate}, Neighbors, false); % State needs to be updated
        
        {receive_sum, ReceivedValues} ->
          {CurrentS, CurrentW} = State,
          {ReceivedS, ReceivedW} = ReceivedValues,
          % io:fwrite("Received S- ~p -> ~p, W- ~p -> ~p in ~p~n", [ReceivedS, CurrentS + ReceivedS, ReceivedW, CurrentW + ReceivedW, self()]),
          if Init == true ->
            ServerPID ! {record_metric};
            true -> pass
          end,
          NewValue = {CurrentS + ReceivedS, CurrentW + ReceivedW},
          {NewS, NewW} = UpdateState(NewValue),
          lists:nth(rand:uniform(length(Neighbors)), Neighbors) ! {receive_sum, {NewS, NewW}},
          io:fwrite("Sent S- ~p , W- ~p in ~p  ~p ~n", [NewS, NewW, self(), NewS / NewW]),
          loop(ServerPID, Algorithm, {{NewS, NewW}, UpdateState, ShouldTerminate}, Neighbors, false); % State needs to be updated

        {announce_death, Pid} -> 
          RemainingNeighbors = lists:filter(fun (Elem) -> Elem =/= Pid end, Neighbors),
          % io:format("Number of neighbors alive ~p~n", [length(RemainingNeighbors)]),
          loop(ServerPID, Algorithm, AlgorithmParams, RemainingNeighbors, Init)
      end
  end.

initialize(ServerPID, Algorithm, AlgorithmParams) ->
  receive 
    {register_neighbours, Neighbors, Node} ->
      io:fwrite("Received neighbors from ~p: ~p~n" , [Node, Neighbors]),
      loop(ServerPID, Algorithm, AlgorithmParams, Neighbors, true); % The inital state must be passed by algorithm. State = {RumourCount, 0}
    
    {register_neighbours, Neighbors, Node, S} ->
      io:fwrite("Received neighbors from ~p: ~p~n" , [Node, Neighbors]),
      {State, UpdateState, ShouldTerminate} = AlgorithmParams,
      loop(ServerPID, Algorithm, {State({S, 1}), UpdateState, ShouldTerminate}, Neighbors, true) % The inital state must be passed by algorithm. State = {RumourCount, 0}
  end.