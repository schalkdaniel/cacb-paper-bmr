cargs = commandArgs(trailingOnly=TRUE)
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

cboost_linear = Compboost$new(dat_noise, "y", loss = LossQuadratic$new())
temp = lapply(cnames[cnames != "y"], function (feat) {
  cboost_linear$addBaselearner(feat, "cat", BaselearnerPolynomial)
})

  temp = capture.output({
  cboost_linear$train(mstop, trace = 0)
})





