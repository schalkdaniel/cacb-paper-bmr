## Add problems and algorithms based on the design
## ===========================================================

round("bla",



for (i in seq_along(TASKS)) {
  resampling = RESAMPLE_SETS[[i]]

  robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")

  ## Split the resample (5-CV) into all folds. Hence, a
  ## task - resampling combination is split into combinations
  ## task - reampling (iter 1), ..., task - resampling (iter K).
  for (k in seq_len(resampling$iters)) {
    ts = robustify$train(TASKS[[i]])[[1]]$clone(deep = TRUE)

    rcustom = rsmp("custom")
    rcustom$instantiate(ts,
      train = list(resampling$train_set(k)),
      test  = list(resampling$test_set(k))
    )
    prob = list(
      task       = ts$clone(deep = TRUE),
      resampling = rcustom
    )
    id = paste0(names(TASKS)[i], "-fold", k)
    addProblem(name = id, data = prob) #, fun = function(job, data) return(data))
  }
}

addAlgorithm(name = "evaluate-learner", fun = function(job, data, instance, lid) {

  lgr::get_logger("bbotk")$set_threshold("trace")
  lgr::get_logger("mlr3")$set_threshold("trace")

  ## Split into train and test:
  task_train = data$task$clone(deep = TRUE)$filter(data$resampling$train_set(1L))
  task_test  = data$task$clone(deep = TRUE)$filter(data$resampling$test_set(1L))

  ## Get base algorithm:
  l  = constructLearner2(lid, raw_learner = TRUE)
  ss = constructSearchSpace(lid, task_train)

  ## Construct tuner and tune:
  tuner = TunerHyperband$new()
  tuner$param_set$values$eta = HYPERBAND_ETA

  inst = TuningInstanceSingleCrit$new(
    task = task_train,
    learner = l,
    resampling = rsmp("cv", folds = RESAMPLING_INNER_FOLDS),
    measure = msr(TUNING_MEASURE),
    search_space = ss,
    terminator = trm("none")
  )

  ## Do we want that? If I assume correctly, this will force hyperband to
  ## use the same design for all folds???
  #set.seed(31415)
  tuner$optimize(inst)

  ## Train learner on complete fold with best pars:
  lf = l$clone(deep = TRUE)
  lf$param_set$values = inst$result_learner_param_vals
  lf$train(task_train)

  ## Predict on test set:
  pred = lf$predict(task_test)

  ## Extract info:
  out = list(archive = inst$archive, test_pred = pred, #msrs_test = pred$score(msrs(SCORE_MEASURES)),
    tuning_results = inst$result)
  return(out)
})

addExperiments(algo.design = list('evaluate-learner' = data.table(lid = c("acc_acwb_b", "acc_hcwb_b", "ebm", "xgboost"))))
