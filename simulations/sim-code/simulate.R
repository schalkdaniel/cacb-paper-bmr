source("R/bm-sim-data.R")
source("R/bm-run.R")

ncores = parallel::detectCores()

## Data configurations:
## ---------------------------------------------------

ns         = c(5000L, 10000L, 20000L, 50000L, 100000L)
ps         = c(5L, 10L, 20L, 50L)
pnoise_rel = c(0.5, 1, 2, 5)
sn_ratio   = c(0.1, 1, 10)
reps       = seq_len(20L)

df_configs = expand.grid(n = ns, p = ps, pnoise_rel = pnoise_rel,
  sn_ratio = sn_ratio, rep = reps)

# Add number of noise features as absolute number:
df_configs$pnoise = trunc(df_configs$p * df_configs$pnoise_rel)
df_configs$pnoise_rel = NULL

## Files of the benchmark scripts:
## ---------------------------------------------------

#bm_dirs = paste0("bm-scripts/binning/", c("runtime", "performance"))
#bm_dirs = "bm-scripts/optimizer/memory"
bm_dirs = paste0("bm-scripts/", c(
  "binning/performance",
  "binning/runtime",
  "binning/memory",
  "optimizer/agbm-mom",
  "optimizer/memory",
  "optimizer/runtime"#,
  #"categorical/runtime",
  #"categorical/memory",
  #"categorical/performance"
))

runBM(df_configs, bm_dirs, cores = ncores)

