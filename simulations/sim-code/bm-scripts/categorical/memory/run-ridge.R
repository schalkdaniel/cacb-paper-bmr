# cargs = commandArgs(trailingOnly=TRUE)
base_dir = "~/repos/bm-CompAspCboost"
base_sub_dir = paste0(base_dir, "/bm-scripts/categorical/memory")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))

library(compboost)

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir, cargs)
load(paste0(base_sub_dir, "/cls_config", cargs, ".Rda"))

seed = trunc(config$n / (config$p + config$pnoise + cls_config$ncls[1] + cls_config$nic[1]) * config$sn_ratio)

set.seed(seed)
dat = simCategoricalData(config$n, config$p, config$pnoise, nclasses = cls_config$ncls[1], ncnoise = cls_config$nic[1])

cnames = colnames(dat$data)
for (fn in cnames[cnames != "y"]) {
  dat$data[[fn]] = as.character(dat$data[[fn]])
}


dat_noise = dat$data

set.seed(seed * config$rep)
dat_noise$y = rnorm(n = config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / config$sn_ratio)



mstop = 200L

## Write compboost code here:
## ------------------------------------

response_ridge = ResponseRegr$new("y", as.matrix(dat_noise$y))

factory_list_ridge = BlearnerFactoryList$new()
temp = lapply(cnames[-which(cnames == "y")], function (fn) {
  cdata_source = CategoricalData$new(dat_noise[[fn]], paste0("x", fn))
  bl = BaselearnerCategoricalRidge$new(cdata_source, list(df = cls_config$ncls[1]))
  factory_list_ridge$registerFactory(bl)
})

optimizer = OptimizerCoordinateDescent$new()

log_iterations = LoggerIteration$new(" iterations", TRUE, mstop)

logger_list = LoggerList$new()
logger_list$registerLogger(log_iterations)

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

temp = capture.output({
  cboost_ridge$train(trace = 0)
})


