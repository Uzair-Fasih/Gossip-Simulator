-module(push_sum).
-export([
  getInitialState/2, 
  updateState/2, 
  shouldTerminate/1, 
  getRumourData/1,
  getStateData/1,
  getSettledState/2
]).

getInitialState(pass, I) -> {I, I, 0, 0}.
updateState(State, pass) -> State;
updateState({S, W, Counter, PrevRatio}, ReceivedData) -> 
  {ReceivedS, ReceivedW, _, _} = ReceivedData,
  
  NewS = S + trunc(ReceivedS),
  NewW = W + trunc(ReceivedW),

  NewRatio = trunc(NewS / NewW),
  
  ShouldUpdateCounter = abs(NewRatio - PrevRatio) < math:pow(10, -10),
  if ShouldUpdateCounter -> {NewS, NewW, Counter + 1, NewRatio};
    true -> {NewS, NewW, 0, NewRatio}
  end.

shouldTerminate({_, _, Counter, _}) -> Counter > 3.
getStateData({S, W, Counter, PrevRatio}) -> {S/2, W/2, Counter, PrevRatio}.
getRumourData(pass) -> pass;
getRumourData({S, W, Counter, PrevRatio}) -> {S/2, W/2, Counter, PrevRatio}.
getSettledState(State, pass) -> State;
getSettledState(_, Other) -> Other.