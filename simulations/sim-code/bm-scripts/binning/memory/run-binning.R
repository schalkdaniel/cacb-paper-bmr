base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/binning/memory")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

library(compboost)

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir)

## Simulate data and create data with noise:
seed = trunc(config$n / (config$p + config$pnoise) * config$sn_ratio)

set.seed(seed)
dat = simData(config$n, config$p, config$pnoise)
dat_noise = dat$data

set.seed(seed * config$rep)
dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)

cnames = colnames(dat_noise)

mstop = 200L

## Write compboost code here:
## ------------------------------------

## binning

cboost_binning = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_binning$addBaselearner(feat, "spline", BaselearnerPSpline, bin_root = 2)
})

temp = capture.output({
  cboost_binning$train(mstop, trace = 0)
})

