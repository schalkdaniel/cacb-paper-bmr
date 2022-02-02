#cargs = commandArgs(trailingOnly=TRUE)
base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/optimizer/memory")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

library(compboost)

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir, cargs)

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

## AGBM

cboost_agbm = Compboost$new(dat_noise, "y", loss = LossQuadratic$new(),
  optimizer = OptimizerAGBM$new(0.0001))
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_agbm$addBaselearner(feat, "spline", BaselearnerPSpline)
})

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

temp = capture.output({
  cboost_agbm$train(mstop, trace = 0)
})


