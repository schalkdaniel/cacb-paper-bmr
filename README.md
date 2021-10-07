# Benchmark code for _Accelerated Component-wise Gradient Boosting using Efficient Data Representation and Momentum-based Optimization_

This repository contains the source code to conduct the simulations/benchmarks, compressed results, as well as the code to analyse/visualize the results.

The structure is as followed:
- `simulations` contains all files for the hypotheses H1 - H4.
- `real-world-bm` contains all files for the empirical questions EQ1 - EQ3

Each folder contains a script `analyse.R` which can be used to reproduce the results and figures from the paper. The `benchmark.R` files can be used to rerun all benchmarks. Be careful when recomputing everything because of very long runtimes of multiple weeks/months depending on the machine.
