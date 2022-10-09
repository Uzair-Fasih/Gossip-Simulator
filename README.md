Run the project
```bash
> erl -make
> erl -pa ebin
> app:start(10, full_network_topology, gossip_algo).
> app:start(10, linear_topology, gossip_algo).
> app:start(9, '2d_grid', gossip_algo).

> app:start(10, full_network_topology, push_sum).
> app:start(10, linear_topology, push_sum).
> app:start(9, '2d_grid', push_sum).
```
