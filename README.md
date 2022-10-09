Run the project
```bash
> erl -make
> erl -pa ebin
> app:start(10, full_network_topology, gossip_algo).
> app:start(16, imperfect_3d_grid, gossip_algo).
```