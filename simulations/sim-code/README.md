# Simulations for "Accelerated Componentwise Gradient Boosting using Efficient Data Representation and Momentum-based Optimization"

To run the complete benchmark run the `simulate.R` script. Be careful, this takes a huge amount of time (depending on the machine multiple weeks).

Furthermore, a few tools must be installed to successfully run the simulation:

- `R` and the packages `compboost` and `foreach`.
- `valgrind` for memory tracking.

Each subfolder contains a `analyse.R` file that can be used to summarize and analyse the results.

Please note, that the results (~ 11 GB) are too much for GitHub. The full results can be downloaded by downloading a docker image.
