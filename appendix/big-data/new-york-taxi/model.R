### Models:
### ===========================================================

ddir = "~/temp/cacb-revision/feasibility/new-york-taxi/"
dat = readRDS(paste0(ddir, "data-merged.Rda"))

table(vapply(dat, class))
object.size(dat) / 1024^3
dim(dat)

# remotes::install_version("BH", version = "1.75")
# remotes::install_github("schalkdaniel/compboost", ref = "c68e8fb32aea862750991260d243cdca1d3ebd0e")

library(compboost)

target = "total_amount"

# No binning:
#mod = boostSplines(data = dat, optimizer = OptimizerCoordinateDescent$new(4L),
  #n_knots = 20L, target = target, iterations = 50L, loss = LossQuadratic$new())

# Binning:
mod = boostSplines(data = dat, optimizer = OptimizerCoordinateDescent$new(4L),
  n_knots = 20L, target = target, iterations = 50L, loss = LossQuadratic$new(), bin_root = 2L)
