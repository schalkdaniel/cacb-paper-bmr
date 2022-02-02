cargs = commandArgs(trailingOnly=TRUE)
base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/categorical/runtime")
config_file = paste0(base_sub_dir, "/config", cargs, ".Rmd")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

library(compboost)

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir, cargs)

msg_log_worker = paste0(as.character(Sys.time()), " >> ", cargs, ": Loading config with:n=", config$n, " p=",
  config$p, " pnoise=", config$pnoise ,"  rep=", config$rep,
  "  signal-to-noise-ratio=", config$sn_rateio)

n_classes = c(5, 10, 20)
p_inf_classes = 0

config_classes = expand.grid(ncls = n_classes, pic = p_inf_classes)
config_classes$nic = trunc(config_classes$ncls * config_classes$pic)
config_classes$pic = NULL


for (i in seq_len(nrow(config_classes))) {

  nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, "-nclasses", config_classes$ncls[i], "-informative-classes", config_classes$nic[i], ".Rda")

  seed = trunc(config$n / (config$p + config$pnoise + config_classes$ncls[i] + config_classes$nic[i]) * config$sn_ratio)

  set.seed(seed)
  dat = simCategoricalData(config$n, config$p, config$pnoise, nclasses = config_classes$ncls[i], ncnoise = config_classes$nic[i])

  cnames = colnames(dat$data)
  for (fn in cnames[cnames != "y"]) {
    dat$data[[fn]] = as.character(dat$data[[fn]])
  }

  dat_noise = dat$data

  set.seed(seed * config$rep)
  dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)

  mstop = 2000L

  msg_log_worker = paste0(msg_log_worker, "\n  ", i, " Create data")

  ## Write compboost code here:
  ## ------------------------------------

  ## Linear base-learner

  time_start_linear = proc.time()

  cboost_linear = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
  temp = lapply(cnames[cnames != "y"], function (feat) {
    cboost_linear$addBaselearner(feat, "cat", BaselearnerPolynomial)
  })

  cboost_linear$addLogger(logger = LoggerTime, use_as_stopper = FALSE, logger_id = "time",
    max_time = 0, time_unit = "seconds")

  time_init_linear = proc.time() - time_start_linear
  temp = capture.output({
    cboost_linear$train(mstop, trace = 0)
  })
  time_fit_linear = proc.time() - time_start_linear + time_init_linear

  msg_log_worker = paste0(msg_log_worker, " - linear")


  ## Binary base-learner

  time_start_binary = proc.time()

  response_binary = ResponseRegr$new("y", as.matrix(dat_noise$y))

  factory_list_binary = BlearnerFactoryList$new()
  temp = lapply(cnames[-which(cnames == "y")], function (fn) {
    cdata_source = CategoricalData$new(dat_noise[[fn]], paste0("x", fn))
    bl_list = lapply(unique(dat_noise[[fn]]), function (cl) {
      bl = BaselearnerCategoricalBinary$new(cdata_source, cl)
      factory_list_binary$registerFactory(bl)
    })
    return (cdata_source)
  })

  optimizer = OptimizerCoordinateDescent$new()

  log_iterations = LoggerIteration$new(" iterations", TRUE, mstop)
  log_time = LoggerTime$new("time", FALSE, 0, "seconds")

  logger_list = LoggerList$new()
  logger_list$registerLogger(log_iterations)
  logger_list$registerLogger(log_time)

  loss_quadratic = LossQuadratic$new()
  cboost_binary = Compboost_internal$new(
    response      = response_binary,
    learning_rate = 0.05,
    stop_if_all_stopper_fulfilled = FALSE,
    factory_list = factory_list_binary,
    loss         = loss_quadratic,
    logger_list  = logger_list,
    optimizer    = optimizer
  )

  time_init_binary = proc.time() - time_start_binary
  temp = capture.output({
    cboost_binary$train(trace = 0)
  })
  time_fit_binary = proc.time() - time_start_binary + time_init_binary

  msg_log_worker = paste0(msg_log_worker, " - binary")


  ## Ridge base-learner

  time_start_ridge = proc.time()

  response_ridge = ResponseRegr$new("y", as.matrix(dat_noise$y))

  factory_list_ridge = BlearnerFactoryList$new()
  temp = lapply(cnames[-which(cnames == "y")], function (fn) {
    cdata_source = CategoricalData$new(dat_noise[[fn]], paste0("x", fn))
    bl = BaselearnerCategoricalRidge$new(cdata_source, list(df = config_classes$ncls[i]))
    factory_list_ridge$registerFactory(bl)
  })

  optimizer = OptimizerCoordinateDescent$new()

  log_iterations = LoggerIteration$new(" iterations", TRUE, mstop)
  log_time = LoggerTime$new("time", FALSE, 0, "seconds")

  logger_list = LoggerList$new()
  logger_list$registerLogger(log_iterations)
  logger_list$registerLogger(log_time)

  loss_quadratic = LossQuadratic$new()
  cboost_ridge = Compboost_internal$new(
    response      = response_ridge,
    learning_rate = 0.05,
    stop_if_all_stopper_fulfilled = FALSE,
    factory_list = factory_list_ridge,
    loss         = loss_quadratic,
    logger_list  = logger_list,
    optimizer    = optimizer
  )

  time_init_ridge = proc.time() - time_start_ridge
  temp = capture.output({
    cboost_ridge$train(trace = 0)
  })
  time_fit_ridge = proc.time() - time_start_ridge + time_init_ridge

  msg_log_worker = paste0(msg_log_worker, " - ridge")


  ## ------------------------------------

  ## Save results:

  bm_extract = list(
    date      = as.character(Sys.time()),
    data_seed = seed,
    config    = config,
    config_classes = config_classes,

    log_linear     = cboost_linear$getLoggerData(),
    time_linear    = c(init = time_init_linear[3], fit = time_fit_linear[3]),

    log_ridge      = cboost_ridge$getLoggerData(),
    time_ridge     = c(init = time_init_ridge[3], fit = time_fit_ridge[3]),

    log_binary     = cboost_binary$getLoggerData(),
    time_binary    = c(init = time_init_binary[3], fit = time_fit_binary[3])
  )

  save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))

  msg_log_worker = paste0(msg_log_worker, " - save")

}

log_file = paste0(base_sub_dir, "/worker_log.txt")
if (file.exists(log_file)) {
  temp = readLines(log_file)
  temp = c(temp, msg_log_worker)
  writeLines(temp, log_file)
} else {
  file.create(log_file)
}

if (file.exists(config_file)) file.remove(config_file)
