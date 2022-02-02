cargs = commandArgs(trailingOnly=TRUE)
base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/optimizer/performance")
config_file = paste0(base_sub_dir, "/config", cargs, ".Rmd")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

config = data.frame(n = 50000L, p = 50, sn_ratio = 1, rep = 1, pnoise = 50)

momentums = c(0.05, 0.1, 0.15)

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
max_mstop = 20000L

eps_for_break = 0
patience = 10L

library(compboost)


## Write compboost code here:
## ------------------------------------

## Coordinate Descent

time_start_cod = proc.time()

cboost_cod = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_cod$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5)
})

cboost_cod$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

oob_response = cboost_cod$prepareResponse(dat$data$y)
oob_data = cboost_cod$prepareData(dat$data)
cboost_cod$addLogger(logger = LoggerOobRisk, use_as_stopper = TRUE, logger_id = "oob_risk",
  used_loss = LossQuadratic$new(), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data,
  oob_response = oob_response)

time_init_cod = proc.time() - time_start_cod
temp = capture.output({
  cboost_cod$train(max_mstop, trace = 0L)
})
time_fit_cod = proc.time() - time_start_cod + time_init_cod



## ------------------------------------

## AGBM

cboost_agbm = Compboost$new(dat_noise, "y", loss = LossQuadratic$new(), optimizer = OptimizerAGBM$new(mom))
time_start_agbm = proc.time()

temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_agbm$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5)
})

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

oob_response = cboost_agbm$prepareResponse(dat$data$y)
oob_data = cboost_agbm$prepareData(dat$data)
cboost_agbm$addLogger(logger = LoggerOobRisk, use_as_stopper = TRUE, logger_id = "oob",
  used_loss = LossQuadratic$new(), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data,
  oob_response = oob_response)

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

time_init_agbm = proc.time() - time_start_agbm
temp = capture.output({
  cboost_agbm$train(max_mstop, trace = 0L)
})
time_fit_agbm = proc.time() - time_start_agbm + time_init_agbm


## ------------------------------------

## Save results:

bm_extract = list(
  date      = as.character(Sys.time()),
  momentum  = mom,
  data_seed = seed,
  config    = config,
  log_cod  = cboost_cod$getLoggerData(),
  risk_cod = cboost_cod$getInbagRisk(),
  time_cod = c(init = time_init_cod[3], fit = time_fit_cod[3]),
  coef_cod = cboost_cod$getEstimatedCoef(),
  trace_cod = cboost_cod$getSelectedBaselearner(),
  log_agbm    = cboost_agbm$getLoggerData(),
  risk_agbm   = cboost_agbm$getInbagRisk(),
  time_agbm   = c(init = time_init_agbm[3], fit = time_fit_agbm[3]),
  coef_agbm   = cboost_agbm$getEstimatedCoef(),
  trace_agbm = cboost_agbm$getSelectedBaselearner(),
  trace_agbm_mom = cboost_agbm$optimizer$getSelectedMomentumBaselearner()
)

save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))

}


#feat = 10

#gg = cboost_cod$plot(paste0("x", feat, "_spline"))

#df_plot = data.frame(x = dat$sim_poly[[feat]]$x, y = dat$sim_poly[[feat]]$y)
#gg + ggplot2::geom_point(data = df_plot, ggplot2::aes(x = x, y = y), alpha = 0.5)
