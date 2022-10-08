-module(gossip).
-export([getInitialState/1, updateState/1, shouldTerminate/1]).

getInitialState({RumourCount}) -> {RumourCount, 0}.
updateState({RumourCount, Count}) -> {RumourCount, Count + 1}.
shouldTerminate({RumourCount, Count}) -> Count > RumourCount.