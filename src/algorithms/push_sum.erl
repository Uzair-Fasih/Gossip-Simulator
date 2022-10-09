-module(push_sum).
-export([setInitialState/1, updatePState/1, shouldTerminatePS/1]).

setInitialState({S, W}) -> {S, W}.
updatePState({S, W}) -> {S / 2, W / 2}.
shouldTerminatePS({S, W}) -> 
    SumEstimate = S / W,
    SumEstimate < math:pow(10, 0). %Arbitrary condition for checking correctness