cargs = commandArgs(trailingOnly=TRUE)
base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/optimizer/runtime")
config_file = paste0(base_sub_dir, "/config", cargs, ".Rmd")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

library(compboost)

momentums = c(0.1)

for (mom in momentums) {

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir, cargs)

nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, "-mom", mom, ".Rda")

## Simulate data and create data with noise:
seed = trunc(config$n / (config$p + config$pnoise) * config$sn_ratio)

set.seed(seed)
dat = simData(config$n, config$p, config$pnoise)
dat_noise = dat$data

set.seed(seed * config$rep)
dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)

cnames = colnames(dat_noise)

mstop = 2000L

## Write compboost code here:
## ------------------------------------

## Coordinate Descent

time_start_cod = proc.time()

cboost_cod = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_cod$addBaselearner(feat, "spline", BaselearnerPSpline)
})

cboost_cod$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

time_init_cod = proc.time() - time_start_cod
temp = capture.output({
  cboost_cod$train(mstop, trace = 0)
})
time_fit_cod= proc.time() - time_start_cod + time_init_cod



## ------------------------------------

## AGBM

time_start_agbm = proc.time()

cboost_agbm = Compboost$new(dat_noise, "y", loss = LossQuadratic$new(), optimizer = OptimizerAGBM$new(mom))
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_agbm$addBaselearner(feat, "spline", BaselearnerPSpline)
})

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

time_init_agbm = proc.time() - time_start_agbm
temp = capture.output({
  cboost_agbm$train(mstop, trace = 0L)
})
time_fit_agbm = proc.time() - time_start_agbm + time_init_agbm




## ------------------------------------

## Save results:

bm_extract = list(
  date      = as.character(Sys.time()),
  momentum = mom,
  data_seed = seed,
  config    = config,
  log_cod  = cboost_cod$getLoggerData(),
  time_cod = c(init = time_init_cod[3], fit = time_fit_cod[3]),
  log_agbm    = cboost_agbm$getLoggerData(),
  time_agbm   = c(init = time_init_agbm[3], fit = time_fit_agbm[3])
)

save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))
}
