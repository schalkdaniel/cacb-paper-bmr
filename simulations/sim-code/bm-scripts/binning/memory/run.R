base_dir = dirname("~/repos/bm-CompAspCboost/run.R")
base_sub_dir = paste0(base_dir, "/bm-scripts/binning/memory")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))
source(paste0(base_dir, "/R/bm-extract-massif.R"))

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir)

## To keep the configurations static we ensure that just the FIRST replication is really done.
## This is because of the high runtime with valgrind as debugger but also due to
## the fact that all replications would have the exact same amount of allocated memory.
if (config$rep == 1) {

   nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, ".Rda")


  ## Simulate data and create data with noise:
  seed = trunc(config$n / (config$p + config$pnoise) * config$sn_ratio)

  ## Run compboost:
  ## -------------------------------------------------------------
  ## compboost is run in seperate files to be able to
  ##qrun it in debugging mode with valgrinds massif as
  ## debugging tool.
  ##
  ## The out file of massif is then read and extracted into
  ## a data.frame with the snapshot data.


  sys_call_base = "R -d \"valgrind --tool=massif --stacks=yes --threshold=0 --detailed-freq=1 --time-unit=B --verbose --trace-children=yes --massif-out-file="

  massif_out_binning = paste0(base_sub_dir, "/massif.out.binning")
  massif_out_nobinning = paste0(base_sub_dir, "/massif.out.nobinning")
  log_file = paste0(base_sub_dir, "/temp-log.txt")

  sys_call_binning   = paste0(sys_call_base, massif_out_binning, " --log-file=", log_file, "\" -e \"source('", base_sub_dir, "/run-binning.R')\"")
  sys_call_nobinning = paste0(sys_call_base, massif_out_nobinning, " --log-file=", log_file, "\" -e \"source('", base_sub_dir, "/run-nobinning.R')\"")

  system(sys_call_binning)

  if (file.exists(massif_out_binning)) {
    ms_extract_binning = extractMassifData(massif_out_binning)
    msg_binning = "done extracting data"
    file.remove(massif_out_binning)
  } else {
    msg_binning = "could not find/create massif file"
    ms_extract_binning = NULL
  }

  system(sys_call_nobinning)

  if (file.exists(massif_out_nobinning)) {
    ms_extract_nobinning = extractMassifData(massif_out_nobinning)
    msg_nobinning = "done extracting data"
    file.remove(massif_out_nobinning)
  } else {
    msg_nobinning = "could not find/create massif file"
    ms_extract_nobinning = NULL
  }

  ## -------------------------------------------------------------

  ## Save results:
  bm_extract = list(
    date      = as.character(Sys.time()),
    data_seed = seed,
    config    = config,
    msg_nobinning = msg_nobinning,
    ms_extract_nobinning = ms_extract_nobinning,
    msg_binning = msg_binning,
    ms_extract_binning = ms_extract_binning
  )

  if (file.exists(log_file)) file.remove(log_file)

  save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))
}

