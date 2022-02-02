base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/binning/runtime")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

library(compboost)

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir)

nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, ".Rda")

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

## No binning

time_start_nobinning = proc.time()

cboost_nobinning = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_nobinning$addBaselearner(feat, "spline", BaselearnerPSpline)
})

cboost_nobinning$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

time_init_nobinning = proc.time() - time_start_nobinning
temp = capture.output({
  cboost_nobinning$train(mstop, trace = 0)
})
time_fit_nobinning = proc.time() - time_start_nobinning + time_init_nobinning



## ------------------------------------

## Binning

time_start_binning = proc.time()

cboost_binning = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_binning$addBaselearner(feat, "spline", BaselearnerPSpline, bin_root = 2)
})

cboost_binning$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

time_init_binning = proc.time() - time_start_binning
temp = capture.output({
  cboost_binning$train(mstop, trace = 0L)
})
time_fit_binning = proc.time() - time_start_binning + time_init_binning




## ------------------------------------

## Save results:

bm_extract = list(
  date      = as.character(Sys.time()),
  data_seed = seed,
  config    = config,
  log_nobinning  = cboost_nobinning$getLoggerData(),
  time_nobinning = c(init = time_init_nobinning[3], fit = time_fit_nobinning[3]),
  log_binning    = cboost_binning$getLoggerData(),
  time_binning   = c(init = time_init_binning[3], fit = time_fit_binning[3])
)

save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))

