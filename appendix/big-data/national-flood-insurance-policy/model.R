### Models:
### ===========================================================

ddir = "~/temp/cacb-revision/feasibility/national-flood-insurance-policy/"
dat = readRDS(paste0(ddir, "nfip-flood-policies.Rda"))

# remotes::install_version("BH", version = "1.75")
# remotes::install_github("schalkdaniel/compboost", ref = "c68e8fb32aea862750991260d243cdca1d3ebd0e")

dat = dat[dat$originalconstructiondate_year < 2010, ]

#dim(dat)
#[1] 14569103       51
#table(sapply(dat, class))
#character    factor   integer   numeric
        #3        19        22         7
#object.size(dat) / 1024^3
#3.4 bytes


cat("Start compboost")
Sys.sleep(5)

library(compboost)

target = "totalinsurancepremiumofthepolicy"

# No binning:
mod = boostSplines(data = dat, optimizer = OptimizerCoordinateDescent$new(4L),
  n_knots = 20L, target = target, iterations = 50L, loss = LossQuadratic$new())

# Binning:
mod = boostSplines(data = dat, optimizer = OptimizerCoordinateDescent$new(4L),
  n_knots = 20L, target = target, iterations = 50L, loss = LossQuadratic$new(),
  bin_root = 2L)

