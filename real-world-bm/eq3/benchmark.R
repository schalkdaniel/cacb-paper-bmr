BM_DIR         = here::here("real-world-bm/eq3/")
SRC_DIR        = paste0(BM_DIR, "R/")
BATCHTOOLS_DIR = paste0(BM_DIR, "batchtools/")

if (FALSE) unlink(BATCHTOOLS_DIR, recursive = TRUE)

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
  suppressMessages(library(xgboost))


  source(paste0(SRC_DIR, "helper.R"))
  source(paste0(SRC_DIR, "setup.R"))
  temp = lapply(FILES, function(f) source(paste0(BM_DIR, f)))
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
      "mlr3pipelines", "mlr3tuning", "compboost", "paradox", "mlr3hyperband", "reticulate"),
    #source = c("helper.R", "classifCompboost.R", "setup.R"),
    source   = FILES,
    seed     = 31415)

  reg$cluster.functions = makeClusterFunctionsInteractive(external = TRUE)
  reg$default.resources = list(
    #walltime = 3600L * 2,
    #memory = 1024L * 16L,
    max.concurrent.jobs = 1L,
    ntasks = 1L,
    ncpus = 1L,
    nodes = 1L
  )

  saveRegistry(reg)

  source(paste0(BM_DIR, "add-experiments.R"))
  submitJobs()
}
