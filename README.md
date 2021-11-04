# Benchmark code for _Accelerated Component-wise Gradient Boosting using Efficient Data Representation and Momentum-based Optimization_

This repository contains the source code to conduct the simulations/benchmarks, compressed results, as well as the code to analyse/visualize the results.

The structure is as followed:
- `simulations` contains all files for the hypotheses H1 - H4.
- `real-world-bm` contains all files for the empirical questions EQ1 - EQ3

Each folder contains a script `analyse.R` which can be used to reproduce the results and figures from the paper. The `benchmark.R` files can be used to rerun all benchmarks. Be careful when recomputing everything because of very long runtimes of multiple weeks/months depending on the machine.

### Run the docker

Running the docker provides an RStudio API in your browswer with all packages pre-installed and data to inspect the results. Therefore do:

1. Get the docker:
  - __Build the docker manually:__ Run `sudo docker build -t schalkdaniel/cacb-paper-bmr .` (You can use whatever tag you like, but for consistency we use `schalkdaniel/cacb-paper-bmr`)
  - __Pull the docker:__ Run `sudo docker pull schalkdaniel/cacb-paper-bmr`
2. Run the docker: `sudo docker run -d -p 8787:8787 -e PASSWORD=test schalkdaniel/cacb-paper-bmr`
3. Open your browser and visit [`localhost:8787`](localhost:8787)
4. Login with `rstudio` as user and `test` as password
