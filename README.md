# Gossip Simulator - DOSP Fall 2022

## Authors
| Name | UFID |
| ----------- | ----------- |
| Mohammed Uzair Fasih | 6286 1020 |
| Sohaib Uddin Syed | 5740 5488 |

## Overview
The goal of the project is to use the actor model in erlang and simulate the gossip algorithm. We have designed our system to work as follows:

## Rubric and Output


## Running the application
1) Install erlang from https://www.erlang.org/ and clone this repository.

2) Install erlang tools.

3) Run the project.

Run the project
```bash
> erl -make
> erl -pa ebin
```

4) To start push_sum, run the following:

```bash
> app:start([no_of_nodes], full_network_topology, push_sum_algo).
> app:start([no_of_nodes], linear_topology, push_sum_algo).
> app:start([no_of_nodes], imperfect_3d_grid, push_sum_algo).
> app:start([no_of_nodes], '2d_topology', push_sum_algo).
```
- ```[no_of_nodes]``` is number of nodes.

5) To start gossip, run the following:

```bash
> erl -make
> erl -pa ebin

> app:start([no_of_nodes], full_network_topology, gossip_algo).
> app:start([no_of_nodes], linear_topology, gossip_algo).
> app:start([no_of_nodes], imperfect_3d_grid, gossip_algo).
> app:start([no_of_nodes], '2d_topology', gossip_algo).
```
- ```[no_of_nodes]``` is number of nodes.
