## PACKAGES:
## =============================================

library(dplyr)
library(tidyr)
library(batchtools)
library(mlr3)

# Important dirs:
BASE_DIR = here::here("real-world-bm/eq1/")
FIG_DIR  = function(fig_name) paste0(BASE_DIR, "/figures/", fig_name)

# We use the results from EQ2 to get the best stopping iterations also
# used for mboost:
BT_DIR   = here::here("real-world-bm/eq2/batchtools")


## HELPER:
## =============================================

source(paste0(BASE_DIR, "R/helper.R"))

## GET RESULTS:
## =============================================

# Load benchmark registry and check status. When loading the
# registry, meta data such as the tasks (in `TASKS`) is loaded
# (warnings are ok, since some functions which are just relevant
# to conduct the benchmark in eq2 were removed and are therefore
# just in the eq2 directory):
loadRegistry(file.dir = BT_DIR, work.dir = BASE_DIR)
getStatus()

# Get results:
jt  = getJobTable(findDone())
res = reduceResultsList(jt)

## PROCESS EQ2 RESULTS:
## =============================================

# This is required because the stopping iterations
# for mboost is obtained by using the optimal
# stop of compboost in EQ2. This is valid, because
# they do not differ in the algorithm (just in
# implementational details).

for (i in seq_along(res)) {
  # Extract task ids from the problem string:
  res[[i]] = cbind(res[[i]],
    fold = as.numeric(gsub("\\D", "", jt$problem[i])),
    task = sub("\\-.*", "", jt$problem[i]),
    learner = jt$algo.pars[[i]]$lid)
  res[[i]][["_iterations"]] = seq_len(nrow(res[[i]]))

  # In compboost, the AUC was transformed to 1 - AUC since
  # stopping is done on minimization. Hence, transform
  # the AUC back:
  if ("riskauc" %in% names(res[[i]]))
    res[[i]]$test_auc  = res[[i]]$riskauc

  res[[i]]$test_auc = 1 - res[[i]]$test_auc
  res[[i]]$val_auc  = 1 - res[[i]]$val_auc
  res[[i]]$seconds  = res[[i]]$time / 1e6
}
df_stop           = do.call(rbind, lapply(res, getStopInfo))
df_stop[["task"]] = factor(df_stop[["task"]], levels = TSKS_SETUP$id)

cwb_stops = df_stop %>% filter(learner == "cwb")

## EQ1:
## =============================================

ll_tt   = list()
idx_run = seq_len(nrow(cwb_stops))

# File to store the data from the benchmark:
dffile = here::here("real-world-bm/eq1/meta/df_mboost.Rda")

# Running this code takes long. Therefore, data is stored in dffile:
if (FALSE) {

  # Check if a results file already exists and load it if so. Is a file
  # exists, just the missing models are run:
  if (file.exists(dffile)) {
    load(dffile)
    for (i in seq_len(nrow(df_mboost))) {
      ll_tt[[i]] = df_mboost[i,]
    }
    # Overwrite the `idx_run` to just run models that were not
    # run:
    idx_run = which(is.na(df_mboost$train_time))
  }

  # Variable to catch error (mboost often fails because of
  # memory issues, we don't want the whole script crash
  # because of one error):
  errs = c()
  for (i in idx_run) {
    message("[", Sys.time(), "] ", i, "/", nrow(cwb_stops))

    # Get the mboost learner (we are using gamboost as compboosts
    # competitor):
    l = lrn("classif.gamboost", mstop = cwb_stops$stop[i])

    # Extract the fold:
    fold = as.integer(substrRight(cwb_stops$fold[i]))

    # Get the training indices from that fold - task combination:
    tset = RESAMPLE_SETS[[cwb_stops$task[i]]]$train_set(fold)

    # Obtain the task by filtering the train indices:
    ts = TASKS[[cwb_stops$task[i]]]$clone(deep = TRUE)$filter(tset)

    # For task 9977, there is a problem that is catched in the other
    # benchmarks via a pipeline. Here, we don't want to overcomplicate
    # thinks, that's why we removed the columns from the task:
    if (as.character(cwb_stops$task[i]) == "9977") {
      # The column names are detected by the code below.
      feats_remove = paste0("V", c(17:18, 41:42, 73:74, 81:86))
      ts = ts$select(setdiff(ts$feature_names, feats_remove))
    }


    ## DETECT FEATURES WHO CRASH THE TRAINING
    #fnames = ts$feature_names
    #for (j in seq_along(fnames)) {
    #  tss = ts$clone(deep = TRUE)$select(fnames[j])

    #  robustify = po("removeconstants", id = "removeconstants_before") %>>%
    #    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    #    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    #    po("collapsefactors", target_level_count = 10) %>>%
    #    po("removeconstants", id = "removeconstants_after")
    #  tss = robustify$train(tss)
    #
    #  l = lrn("classif.gamboost", mstop = 1)
    #  e = try(l$train(tss[[1]]), silent = TRUE)
    #  if (inherits(e, "try-error")) message("ERRROR: ", fnames[j])
    #}

    # Define robustify pipeline to do basic pre processing:
    robustify = po("removeconstants", id = "removeconstants_before") %>>%
      po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
      po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
      po("collapsefactors", target_level_count = 10) %>>%
      po("removeconstants", id = "removeconstants_after")
    ts = robustify$train(ts)

    e = NULL
    e = try({ l$train(ts[[1]]); TRUE }, silent = TRUE)
    if (inherits(e, "try-error")) {
      # If an error occurs, write a dummy entry and print the error:
      ll_tt[[i]] = data.frame(learner = "mboost", train_time = NA,
        task = cwb_stops$task[i], fold = fold, mstop = cwb_stops$stop[i])
      errs = c(errs, paste0("ERROR: ", attr(e, "condition")$message))
      msg  = last(errs)
    } else {
      # If not save the actual time mboost requires and print a message:
      ll_tt[[i]] = data.frame(learner = "mboost", train_time = l$state$train_time,
        task = cwb_stops$task[i], fold = fold, mstop = cwb_stops$stop[i])
      msg = paste0("FINISH training model in ", l$state$train_time, " seconds")
    }
    message("[", Sys.time(), "] ", i, "/", nrow(cwb_stops), ": ", msg)
  }
  df_mboost = do.call(rbind, ll_tt)

  # Save the data frame:
  save(df_mboost, file = paste0(BASE_DIR, "meta/df_mboost.Rda"))

  # Also save df_stop for later use:
  save(df_stop, file = paste0(BASE_DIR, "meta/df_stop.Rda"))
}
