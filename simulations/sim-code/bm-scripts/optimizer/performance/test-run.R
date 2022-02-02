base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/optimizer/performance")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

config = data.frame(n = 50000L, p = 50, sn_ratio = 1, rep = 1, pnoise = 50)

#momentums = c(0.05, 0.1, 0.15)



## Simulate data and create data with noise:
seed = trunc(config$n / (config$p + config$pnoise) * config$sn_ratio)

set.seed(seed)
dat = simData(config$n, config$p, config$pnoise)
dat_noise = dat$data

set.seed(seed * config$rep)
dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)

sd(dat_noise$y)
sd(dat$data$y)

cnames = colnames(dat_noise)
max_mstop = 100000L

eps_for_break = 0
patience = 10L

library(compboost)


## Write compboost code here:
## ------------------------------------

## Coordinate Descent
rerun_cod = FALSE
if (rerun_cod) {
  cboost_cod = Compboost$new(dat_noise, "y", loss = LossQuadratic$new(), optimizer = OptimizerCoordinateDescent$new(7L))
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

  cboost_cod$train(max_mstop, trace = 200L)
}



## ------------------------------------

mom = 0.00001
agbm_early_stop = TRUE
reset_agbm_mstop = FALSE
if (reset_agbm_mstop) max_mstop = 20000L

cboost_agbm = Compboost$new(dat_noise, "y", loss = LossQuadratic$new(), optimizer = OptimizerAGBM$new(mom, 7L))
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_agbm$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5)
})

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

oob_response = cboost_agbm$prepareResponse(dat$data$y)
oob_data = cboost_agbm$prepareData(dat$data)
cboost_agbm$addLogger(logger = LoggerOobRisk, use_as_stopper = agbm_early_stop, logger_id = "oob",
  used_loss = LossQuadratic$new(), eps_for_break = eps_for_break, patience = patience, oob_data = oob_data,
  oob_response = oob_response)

cboost_agbm$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
  max_time = 0, time_unit = "seconds")

cboost_agbm$train(max_mstop, trace = if (reset_agbm_mstop) trunc(max_mstop / 4) else 200L)



## Save results:

bm_extract = list(
  date      = as.character(Sys.time()),
  momentum  = mom,
  data_seed = seed,
  config    = config,
  log_cod  = cboost_cod$getLoggerData(),
  risk_cod = cboost_cod$getInbagRisk(),
  coef_cod = cboost_cod$getEstimatedCoef(),
  trace_cod = cboost_cod$getSelectedBaselearner(),
  log_agbm    = cboost_agbm$getLoggerData(),
  risk_agbm   = cboost_agbm$getInbagRisk(),
  coef_agbm   = cboost_agbm$getEstimatedCoef(),
  trace_agbm = cboost_agbm$getSelectedBaselearner(),
  trace_agbm_mom = cboost_agbm$optimizer$getSelectedMomentumBaselearner()
)

getFeatEffectData = function (bm_extract, bl, truth = TRUE)
{
  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)

  #bl_tab = table(bm_extract$trace_cod)
  #bl = names(which.max(bl_tab))
  bl_nbr = as.numeric(gsub("\\D", "", bl))

  coefs_cod = bm_extract$coef_cod[[bl]]
  coefs_agbm = bm_extract$coef_agbm[[bl]]

  x = dat$data[[paste0("x", bl_nbr)]]
  y = dat$sim_poly[[bl_nbr]]$y

  knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
  basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

  pred_cod = basis %*% coefs_cod
  pred_agbm = basis %*% coefs_agbm

  if (truth) {
    out = data.frame(
      x = rep(x, 3),
      y = c(y - mean(y), pred_cod - mean(pred_cod), pred_agbm -mean(pred_agbm)),
      method = rep(c("truth", "cod", "agbm"), each = length(x))
    )
  } else {
    out = data.frame(
      x = rep(x, 2),
      y = c(pred_cod - mean(pred_cod), pred_agbm - mean(pred_agbm)),
      method = rep(c("cod", "agbm"), each = length(x))
    )
  }
  return (out)
}

oob_cod = cboost_cod$getLoggerData()[,2]
oob_agbm = cboost_agbm$getLoggerData()[,2]
oob = data.frame(iter = c(seq_along(oob_cod), seq_along(oob_agbm)),
  oob = c(oob_cod, oob_agbm), optimizer = rep(c("cod", "agbm"), times = c(length(oob_cod), length(oob_agbm))))
oob_label = data.frame(x = c(length(oob_cod), length(oob_agbm)), y = c(min(oob_cod), min(oob_agbm)),
  optimizer = c("cod", "agbm"))

library(ggplot2)

gg_oob = ggplot() +
  geom_line(data = oob, mapping = aes(x = iter, y = oob, color = optimizer)) +
  geom_label(data = oob_label, mapping = aes(x = x, y = y, label = as.character(round(y, 4)), fill = optimizer),
    color = "white", fontface = "bold") +
  ggtitle(paste0("Momentum: ", mom))


coefs_cod = cboost_cod$getEstimatedCoef()
coefs_agbm = cboost_agbm$getEstimatedCoef()

means = c()
for (nm in names(coefs_cod)) {
  if (nm %in% names(coefs_agbm)) {
    means = c(means, mean((coefs_cod[[nm]] - coefs_agbm[[nm]])^2))
  }
}
means

tbl_agbm = sort(table(c(bm_extract$trace_agbm, bm_extract$trace_agbm_mom)), decreasing = TRUE)
n_plot = 9L
bls = names(tbl_agbm[seq_len(n_plot)])

ggs = lapply(bls, function (bl) {
  df_plot = getFeatEffectData(bm_extract, bl)
  ggplot(data = df_plot, aes(x = x, y = y, color = method)) + geom_line() + ggtitle(bl)
})
gg_bl = gridExtra::grid.arrange(grobs = ggs, ncol = sqrt(n_plot))


gg_oob
#plot(gg_bl)











