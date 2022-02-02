library(compboost)

file_data_buzz = "~/Downloads/php9VSzX6.csv"
dat = read.csv(file_data_buzz)

target = "Annotation"
momentum = 0.00001
cores = parallel::detectCores() - 1L
cores = 4L
cnames = names(dat)

cboost_binning = Compboost$new(dat, target, loss = LossQuadratic$new(),
  optimizer = OptimizerAGBM$new(momentum, cores))
temp = lapply(cnames[cnames != target], function (feat) {
  cboost_binning$addBaselearner(feat, "spline", BaselearnerPSpline, df = 5,  bin_root = 2)
})


mstop = 20
cboost_binning$train(mstop, trace = mstop / 10L)


f_char = paste0(target, " ~ ", paste(paste0("bbs(", cnames[cnames != target], ", df = 5)"), collapse = " + "))
mod = mboost(as.formula(f_char), data = dat, control = boost_control(mstop = mstop, nu = 0.05))
