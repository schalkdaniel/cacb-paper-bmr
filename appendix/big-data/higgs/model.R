### Models:
### ===========================================================

dat = data.table::fread("~/temp/cacb-revision/feasibility/higgs/HIGGS.csv")

Sys.sleep(5)

#dim(dat)
#[1] 11000000       29
#object.size(dat) / 1024^3
#[1] 11000000       29

# remotes::install_version("BH", version = "1.75")
# remotes::install_github("schalkdaniel/compboost", ref = "c68e8fb32aea862750991260d243cdca1d3ebd0e")

library(compboost)

target = "V1"
dat[[target]] = as.factor(target)

## No binning:
#mod = boostSplines(data = dat, optimizer = OptimizerCoordinateDescent$new(4L),
  #n_knots = 20L, target = target, iterations = 50L, loss = LossBinomial$new())

## Binning:
mod = boostSplines(data = dat, optimizer = OptimizerCoordinateDescent$new(4L),
  n_knots = 20L, target = target, iterations = 50L, loss = LossBinomial$new(),
  bin_root = 2L)

