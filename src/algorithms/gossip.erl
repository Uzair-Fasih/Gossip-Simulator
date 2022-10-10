-module(gossip).
-export([
  getInitialState/2, 
  updateState/2, 
  shouldTerminate/1, 
  getRumourData/1,
  getStateData/1,
  getSettledState/2
]).

getInitialState({RumourCount}, _) -> {RumourCount, 0}.
updateState({RumourCount, Count}, _) -> {RumourCount, Count + 1}.
shouldTerminate({RumourCount, Count}) -> Count > RumourCount.
getRumourData(_) -> pass.
getStateData(_) -> pass.
getSettledState(State, pass) -> State.