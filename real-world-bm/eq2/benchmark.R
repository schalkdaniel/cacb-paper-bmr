BM_DIR         = here::here("real-world-bm/eq1-2/")
SRC_DIR        = paste0(BM_DIR, "R/")
BATCHTOOLS_DIR = paste0(BM_DIR, "batchtools/")

if (! dir.exists(BATCHTOOLS_DIR)) {
  suppressMessages(library(data.table))
  suppressMessages(library(R6))
  suppressMessages(library(mlr3))
  suppressMessages(library(mlr3tuning))
  suppressMessages(library(mlrintermbo))
  suppressMessages(library(mlr3learners))
  suppressMessages(library(mlr3extralearners))
  suppressMessages(library(mlr3pipelines))
  suppressMessages(library(paradox))

  source(paste0(SRC_DIR, "helper.R"))
  source(paste0(SRC_DIR, "setup.R"))
  nuisance = lapply(FILES, function(f) source(paste0(BM_DIR, f)))
}


## Batchtools
## ===========================================================

library(batchtools)

if (dir.exists(BATCHTOOLS_DIR)) {

  loadRegistry(BATCHTOOLS_DIR, writeable = TRUE, work.dir = BM_DIR)
  submitJobs(findNotDone())

} else {

  reg = makeExperimentRegistry(
    file.dir = BATCHTOOLS_DIR,
    packages = c("data.table", "R6", "mlr3", "mlr3learners", "mlr3extralearners",
      "mlr3pipelines", "mlr3tuning", "compboost", "paradox"),
    source   = FILES,
    seed     = 31415)

  reg$cluster.functions = makeClusterFunctionsInteractive(external = TRUE)
  reg$default.resources = list(
    max.concurrent.jobs = 1L,
    ntasks = 1L,
    ncpus = 1L,
    nodes = 1L
  )

  saveRegistry(reg)

  source(paste0(SRC_DIR, "add-experiments.R"))
  submitJobs()
}
