cargs = commandArgs(trailingOnly=TRUE)
base_dir = dirname("~/repos/bm-CompAspCboost/run.R")
base_sub_dir = paste0(base_dir, "/bm-scripts/categorical/memory")

config_file = paste0(base_sub_dir, "/config", cargs, ".Rmd")

source(paste0(base_dir, "/R/bm-sim-data.R"))
source(paste0(base_dir, "/R/bm-run.R"))
source(paste0(base_dir, "/R/bm-extract-massif.R"))

## Load configuration and paste name of output file
config = loadConfig(base_sub_dir, cargs)

msg_log_worker = paste0(as.character(Sys.time()), " >> ", cargs, ": Loading config with:n=", config$n, " p=",
  config$p, " pnoise=", config$pnoise ,"  rep=", config$rep,
  "  signal-to-noise-ratio=", config$sn_rateio)

n_classes = c(5, 10, 20)
p_inf_classes = 0

config_classes = expand.grid(ncls = n_classes, pic = p_inf_classes)
config_classes$nic = trunc(config_classes$ncls * config_classes$pic)
config_classes$pic = NULL


## To keep the configurations static we ensure that just the FIRST replication is really done.
## This is because of the high runtime with valgrind as debugger but also due to
## the fact that all replications would have the exact same amount of allocated memory.
if (config$rep == 1) {

  for (i in seq_len(nrow(config_classes))) {
    nm_save = paste0("xxx-n", config$n, "-p", config$p, "-pnoise", config$pnoise, "-snr", config$sn_ratio, "-rep", config$rep, "-nclasses", config_classes$ncls[i], "-informative-classes", config_classes$nic[i], ".Rda")

    seed = trunc(config$n / (config$p + config$pnoise + config_classes$ncls[i] + config_classes$nic[i]) * config$sn_ratio)

    cls_config = config_classes[i,]
    save(cls_config, file = paste0(base_sub_dir, "/cls_config", cargs, ".Rda"))


    ## Run compboost:
    ## -------------------------------------------------------------
    ## compboost is run in seperate files to be able to
    ##qrun it in debugging mode with valgrinds massif as
    ## debugging tool.
    ##
    ## The out file of massif is then read and extracted into
    ## a data.frame with the snapshot data.


    sys_call_base = "R -d \"valgrind --tool=massif --stacks=yes --threshold=0 --detailed-freq=1 --time-unit=B --verbose --trace-children=yes --massif-out-file="

    massif_out_linear= paste0(base_sub_dir, "/massif.out.linear", cargs)
    massif_out_binary= paste0(base_sub_dir, "/massif.out.binary", cargs)
    massif_out_ridge= paste0(base_sub_dir, "/massif.out.ridge", cargs)
    log_file = paste0(base_sub_dir, "/temp-log", cargs, ".txt")

    sys_call_linear = paste0(sys_call_base, massif_out_linear, " --log-file=", log_file, "\" -e \"cargs = ", cargs, ";source('", base_sub_dir, "/run-linear.R')\"")
    sys_call_binary = paste0(sys_call_base, massif_out_binary, " --log-file=", log_file, "\" -e \"cargs = ", cargs, ";source('", base_sub_dir, "/run-binary.R')\"")
    sys_call_ridge = paste0(sys_call_base, massif_out_ridge, " --log-file=", log_file, "\" -e \"cargs = ", cargs, ";source('", base_sub_dir, "/run-ridge.R')\"")

    system(sys_call_linear)

    if (file.exists(massif_out_linear)) {
      ms_extract_linear= extractMassifData(massif_out_linear)
      msg_linear= "done extracting data"
      file.remove(massif_out_linear)
    } else {
      msg_linear= "could not find/create massif file"
      ms_extract_linear = NULL
    }
    msg_log_worker = paste0(msg_log_worker, "\n  ", i, " profiling linear")

    system(sys_call_binary)

    if (file.exists(massif_out_binary)) {
      ms_extract_binary = extractMassifData(massif_out_binary)
      msg_binary = "done extracting data"
      file.remove(massif_out_binary)
    } else {
      msg_binary = "could not find/create massif file"
      ms_extract_binary = NULL
    }
    msg_log_worker = paste0(msg_log_worker, " - binary")


    system(sys_call_ridge)

    if (file.exists(massif_out_ridge)) {
      ms_extract_ridge = extractMassifData(massif_out_ridge)
      msg_ridge = "done extracting data"
      file.remove(massif_out_ridge)
    } else {
      msg_ridge = "could not find/create massif file"
      ms_extract_ridge = NULL
    }
    msg_log_worker = paste0(msg_log_worker, " - ridge")

    ## -------------------------------------------------------------

    ## Save results:
    bm_extract = list(
      date      = as.character(Sys.time()),
      data_seed = seed,
      config    = config,
      cls_config = cls_config,

      msg_linear = msg_linear,
      ms_extract_linear = ms_extract_linear,

      msg_binary = msg_binary,
      ms_extract_binary = ms_extract_binary,

      msg_ridge = msg_ridge,
      ms_extract_ridge = ms_extract_ridge
    )

    if (file.exists(log_file)) file.remove(log_file)
    if (file.exists(paste0(base_sub_dir, "/cls_config", cargs, ".Rda"))) file.remove(paste0(base_sub_dir, "/cls_config", cargs, ".Rda"))

    save(bm_extract, file = paste0(base_sub_dir, "/", nm_save))
    msg_log_worker = paste0(msg_log_worker, " - save")
  }
}

log_file = paste0(base_sub_dir, "/worker_log.txt")
if (file.exists(log_file)) {
  temp = readLines(log_file)
  temp = c(temp, msg_log_worker)
  writeLines(temp, log_file)
} else {
  file.create(log_file)
}

if (file.exists(config_file)) file.remove(config_file)

