{
  application, gossip,
  [
    {vsn, "1.0.0"},
    {description, "Write the gossip simulator through the usage of actor model in Erlang. Project for the class of DOSP Fall 2022 at University of Florida"},
    {modules, [app, node, full_network]},
    {env, [{rumourCount, 10}]}
  ]
}.