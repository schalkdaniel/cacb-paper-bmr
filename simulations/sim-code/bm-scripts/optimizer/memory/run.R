cargs = commandArgs(trailingOnly=TRUE)
base_dir = dirname("~/repos/bm-CompAspCboost/run.R")
base_sub_dir = paste0(base_dir, "/bm-scripts/optimizer/memory")

config_file = paste0(base_sub_dir, "/config", cargs, ".Rmd")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))
source(paste0(base_dir, "/R/bm-extract-massif.R"))

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir, cargs)

## To keep the configurations static we ensure that just the FIRST replication is really done.
## This is because of the high runtime with valgrind as debugger but also due to
## the fact that all replications would have the exact same amount of allocated memory.
if (config$rep == 1) {

   nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, ".Rda")


  ## Simulate data and create data with noise:
  seed = trunc(config$n / (config$p + config$pnoise) * config$sn_ratio)

  ## Run compboost:
  ## -------------------------------------------------------------
  ## compboost is run in separate files to be able to
  ## run it in debugging mode with valgrinds massif as
  ## debugging tool.
  ##
  ## The out file of massif is then read and extracted into
  ## a data.frame with the snapshot data.


  sys_call_base = "R -d \"valgrind --tool=massif --stacks=yes --threshold=0 --detailed-freq=1 --time-unit=B --verbose --trace-children=yes --massif-out-file="

  #massif_out_cod = paste0(base_sub_dir, "/massif.out.cod", cargs)
  massif_out_agbm = paste0(base_sub_dir, "/massif.out.agbm", cargs)
  log_file = paste0(base_sub_dir, "/temp-log", cargs, ".txt")

  #sys_call_cod   = paste0(sys_call_base, massif_out_cod, " --log-file=", log_file, "\" -e \"source('", base_sub_dir, "/run-cod.R')\"")
  sys_call_agbm = paste0(sys_call_base, massif_out_agbm, " --log-file=", log_file, "\" -e \"cargs=", cargs, ";source('", base_sub_dir, "/run-agbm.R')\"")


  ## NOT NECESSARY SINCE ALREADY DONE IN BINNING!
  #system(sys_call_cod)

  #if (file.exists(massif_out_cod)) {
    #ms_extract_cod = extractMassifData(massif_out_cod)
    #msg_cod = "done extracting data"
    #file.remove(massif_out_cod)
  #} else {
    #msg_cod = "could not find/create massif file"
    #ms_extract_cod = NULL
  #}

  system(sys_call_agbm)

  if (file.exists(massif_out_agbm)) {
    ms_extract_agbm = extractMassifData(massif_out_agbm)
    msg_agbm = "done extracting data"
    file.remove(massif_out_agbm)
  } else {
    msg_agbm = "could not find/create massif file"
    ms_extract_agbm = NULL
  }

  ## -------------------------------------------------------------

  ## Save results:
  bm_extract = list(
    date      = as.character(Sys.time()),
    data_seed = seed,
    config    = config,
    msg_agbm = msg_agbm,
    ms_extract_agbm = ms_extract_agbm#,
    #msg_cod = msg_cod,
    #ms_extract_cod = ms_extract_cod
  )

  if (file.exists(log_file)) file.remove(log_file)

  save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))
}

