cat("[", as.character(Sys.time()),   "] Loading new learner LearnerClassifHCWB\n", sep = "")

LearnerClassifHCWB = R6Class("LearnerClassifHCWB",
  inherit = LearnerClassif,
  public = list(

    transition = NULL,
    iter = NULL,

    #' @description
    #' Create a `LearnerClassifHCWB` object.
    initialize = function() {
      ps = ParamSet$new(
        params = list(
          ParamInt$new(id = "mstop", default = 100L, lower = 1L),
          ParamDbl$new(id = "learning_rate", default = 0.05, lower = 0),
          ParamDbl$new(id = "df", default = 5, lower = 1),
          ParamDbl$new(id = "df_cat", default = 1, lower = 1),
          ParamDbl$new(id = "n_knots", default = 20L, lower = 4),
          ParamLgl$new(id = "df_autoselect", default = FALSE),
          ParamInt$new(id = "ncores", default = 1L, lower = 1L, upper = parallel::detectCores()),

          ParamDbl$new(id = "oob_fraction", default = 0, lower = 0, upper = 0.9),
          ParamLgl$new(id = "use_stopper", default = TRUE),
          ParamLgl$new(id = "just_log", default = TRUE),
          ParamLgl$new(id = "use_stopper_auc", default = FALSE),
          ParamLgl$new(id = "just_log_auc", default = TRUE),
          ParamInt$new(id = "patience", default = 10, lower = 1),
          ParamDbl$new(id = "eps_for_break", default = 0.00001),

          ParamDbl$new(id = "momentum", default = 0.05, lower = 0),

          ParamFct$new(id = "bin_method", default = "linear", levels = c("linear", "quantile")),
          ParamDbl$new(id = "bin_root", default = 0, lower = 0, upper = 4),

          ParamLgl$new(id = "show_output", default = TRUE),
          ParamInt$new(id = "oob_seed", default = sample(seq_len(1e6), 1), lower = 1L),
          ParamUty$new(id = "additional_auc_task", default = list())
        )
      )
      super$initialize(
        id = "classif.HCWB",
        packages = "compboost",
        feature_types = c("numeric", "factor", "integer", "character"),
        predict_types = c("response", "prob"),
        param_set = ps,
        properties = c("twoclass")
      )
    },
    setToIteration = function(iter) {
      if (is.null(self$transition)) {
        stop("No trained model!")
      } else {
        if (self$transition < iter) {
          if ("cboost_restart" %in% names(self$model))
            self$model$cboost_restart$train(iter - self$transition)
        } else {
          self$model$cboost$train(iter)
        }
        self$iter = iter
      }
    }
  ),

  private = list(
    .train = function(task) {

      ## Merge default hyperparameter with the specified ones:
      self$param_set$values = mlr3misc::insert_named(self$param_set$default, self$param_set$values)

      ## Check number of cores:
      ncores = checkCores(task, self$param_set$values$ncores)

      use_validation = FALSE
      ## Split data into train and validation:
      if (self$param_set$values$oob_fraction > 0) {
        oobf = self$param_set$values$oob_fraction

        set.seed(self$param_set$values$oob_seed)
        val_idx   = sample(seq_len(task$nrow), trunc(oobf * task$nrow))
        train_idx = setdiff(seq_len(task$nrow), val_idx)

        ttask = task$clone(deep = TRUE)$filter(train_idx)
        vtask = task$clone(deep = TRUE)$filter(val_idx)
      } else {
        stop("hCWB requires the specification of an oob fraction for stopping ACWB.")
      }
      if (self$param_set$values$use_stopper + self$param_set$values$use_stopper_auc == 0)
        stop("Specify a stopper! Set 'set_stopper' or 'set_stopper_auc' to 'TRUE'.")

      ## Automatically select degrees of freedom:
      if (self$param_set$values$df_autoselect) {
        dfs = dfAutoselect(task, self$param_set$values$df, self$param_set$values$n_knots)

        self$param_set$values$df     = dfs$num
        self$param_set$values$df_cat = dfs$cat
      }


      ## Define optimizer:
      optimizer = compboost::OptimizerAGBM$new(self$param_set$values$momentum, ncores)

      out  = list()

      ## Build compboost model:
      model = Compboost$new(
        data = ttask$data(),
        target = ttask$target_names,
        optimizer = optimizer,
        loss = LossBinomial$new(),
        learning_rate = self$param_set$values$learning_rate)

      for (feat in ttask$feature_names) {
        if (is.numeric(ttask$data()[[feat]])) {
          model$addBaselearner(feat, "spline", BaselearnerPSpline, InMemoryData,
            degree = 3,
            n_knots = self$param_set$values$n_knots,
            df = self$param_set$values$df,
            differences = 2,
            bin_root = self$param_set$values$bin_root,
            bin_method = "linear",
            cache_type = "inverse")
        } else {
          checkmate::assertNumeric(self$param_set$values$df_cat, len = 1L, lower = 1)
          if (length(unique(feat)) > self$param_set$values$df_cat)
            stop("Categorical degree of freedom must be smaller than the number of classes (here <",
              length(unique(feat)), ")")
          model$addBaselearner(feat, "ridge", BaselearnerCategoricalRidge, InMemoryData, df = self$param_set$values$df_cat)
        }
      }
      model$addLogger(LoggerTime, FALSE, "time", 0, "microseconds")

      if (self$param_set$values$use_stopper) {
        model$addLogger(logger = LoggerOobRisk,
          use_as_stopper = !self$param_set$values$just_log,
          logger_id      = "oob_risk",
          used_loss      = LossBinomial$new(),
          esp_for_break  = self$param_set$values$eps_for_break,
          patience       = self$param_set$values$patience,
          oob_data       = model$prepareData(vtask$data()),
          oob_response   = model$prepareResponse(vtask$data()[[vtask$target_names]]))
      }
      if (self$param_set$values$use_stopper_auc) {
        model$addLogger(logger = LoggerOobRisk,
          use_as_stopper = self$param_set$values$use_stopper_auc,
          logger_id      = "val_auc",
          used_loss      = getAUCLoss(),
          esp_for_break  = self$param_set$values$eps_for_break,
          patience       = self$param_set$values$patience,
          oob_data       = model$prepareData(vtask$data()),
          oob_response   = model$prepareResponse(vtask$data()[[vtask$target_names]]))
      }
      if (inherits(self$param_set$values$additional_auc_task, "Task")) {
        ts = self$param_set$values$additional_auc_task
        model$addLogger(logger = LoggerOobRisk,
          use_as_stopper = FALSE,
          logger_id      = "test_auc",
          used_loss      = getAUCLoss(),
          esp_for_break  = self$param_set$values$eps_for_break,
          patience       = self$param_set$values$patience,
          oob_data       = model$prepareData(ts$data()),
          oob_response   = model$prepareResponse(ts$data()[[ttask$target_names]]))
      }

      model$train(self$param_set$values$mstop)

      iters = length(model$getSelectedBaselearner())

      if (iters == self$param_set$values$mstop) {
        out$cboost = model
        self$iter  = iters
        return(out)
      }

      ### Reset iterations if early stopping was used:
      if (iters < self$param_set$values$mstop) {
        if (iters <= (self$param_set$values$patience + 1)) {
          out$cboost      = NULL
          self$transition = 0
          iters           = 0

          reloss = LossBinomial$new()
        } else {
          iters = iters - (self$param_set$values$patience + 1)
          model$train(iters)

          self$transition = iters
          out$cboost      = model

          treinit  = model$predict(ttask$data())
          vreinit  = model$predict(vtask$data())
          reloss   = LossBinomial$new(treinit, TRUE)
        }
      }

      ## Define CWB parts
      optimizer = compboost::OptimizerCoordinateDescent$new(ncores)

      ## Build compboost model:
      remodel = Compboost$new(
        data = ttask$data(),
        target = ttask$target_names,
        optimizer = optimizer,
        loss = reloss,
        learning_rate = self$param_set$values$learning_rate)

      for (feat in ttask$feature_names) {
        if (is.numeric(ttask$data()[[feat]])) {
          remodel$addBaselearner(feat, "spline", BaselearnerPSpline, InMemoryData,
            degree = 3,
            n_knots = self$param_set$values$n_knots,
            df = self$param_set$values$df,
            differences = 2,
            bin_root = self$param_set$values$bin_root,
            bin_method = "linear",
            cache_type = "inverse")
        } else {
          checkmate::assertNumeric(self$param_set$values$df_cat, len = 1L, lower = 1)
          if (length(unique(feat)) > self$param_set$values$df_cat)
            stop("Categorical degree of freedom must be smaller than the number of classes (here <",
              length(unique(feat)), ")")
          remodel$addBaselearner(feat, "ridge", BaselearnerCategoricalRidge, InMemoryData, df = self$param_set$values$df_cat)
        }
      }
      remodel$addLogger(LoggerTime, FALSE, "time", 0, "microseconds")

      if (self$param_set$values$use_stopper) {
        if (self$transition == 0)
          reloss = LossBinomial$new()
        else
          reloss = LossBinomial$new(model$predict(vtask$data()), TRUE)

        remodel$addLogger(logger = LoggerOobRisk,
          use_as_stopper = !self$param_set$values$just_log,
          logger_id      = "oob_risk",
          used_loss      = reloss,
          esp_for_break  = self$param_set$values$eps_for_break,
          patience       = self$param_set$values$patience,
          oob_data       = model$prepareData(vtask$data()),
          oob_response   = model$prepareResponse(vtask$data()[[ttask$target_names]]))
      }
      if (self$param_set$values$use_stopper_auc) {
        if (self$transition == 0)
          reloss = getAUCLoss()
        else
          reloss = getAUCLossInit(model$predict(vtask$data()))

        remodel$addLogger(logger = LoggerOobRisk,
          use_as_stopper = !self$param_set$values$just_log_auc,
          logger_id      = "val_auc",
          used_loss      = reloss,
          esp_for_break  = self$param_set$values$eps_for_break,
          patience       = self$param_set$values$patience,
          oob_data       = model$prepareData(vtask$data()),
          oob_response   = model$prepareResponse(vtask$data()[[ttask$target_names]]))
      }
      if (inherits(self$param_set$values$additional_auc_task, "Task")) {
        ts = self$param_set$values$additional_auc_task
        if (self$transition == 0)
          reloss = getAUCLoss()
        else
          reloss = getAUCLossInit(model$predict(ts$data()))

        remodel$addLogger(logger = LoggerOobRisk,
          use_as_stopper = FALSE,
          logger_id      = "test_auc",
          used_loss      = reloss,
          esp_for_break  = self$param_set$values$eps_for_break,
          patience       = self$param_set$values$patience,
          oob_data       = model$prepareData(ts$data()),
          oob_response   = model$prepareResponse(ts$data()[[ttask$target_names]]))
      }

      remodel$train(self$param_set$values$mstop - iters)
      out$cboost_restart = remodel

      reiters = length(remodel$getSelectedBaselearner())
      iters   = iters + reiters

      ### Reset iterations if early stopping was used:
      if (iters < self$param_set$values$mstop) {
        if (reiters <= (self$param_set$values$patience + 1)) {
          out$cboost      = NULL
          self$transition = 0
        } else {
          iters = iters - (self$param_set$values$patience + 1)
          model$train(iters)
        }
      }
      self$iter = iters
      return(out)
    },

    .predict = function(task) {
      newdata = task$data(cols = task$feature_names)

      if (is.null(self$model$cboost))
        lin_pred = rep(0, nrow(newdata))
      else
        lin_pred = self$model$cboost$predict(newdata)

      if (("cboost_restart" %in% names(self$model)) && (self$transition < self$iter))
        lin_pred = lin_pred + self$model$cboost_restart$predict(newdata)

      probs = 1 / (1 + exp(-lin_pred))

      pos = self$model$cboost$response$getPositiveClass()
      neg = setdiff(names(self$model$cboost$response$getClassTable()), pos)
      pmat = matrix(c(probs, 1 - probs), ncol = 2L, nrow = length(probs))
      colnames(pmat) = c(pos, neg)
      if (self$predict_type == "prob") {
        return(list(prob = pmat))
      }
      if (self$predict_type == "response") {
        return(list(response = ifelse(probs > self$model$cboost$response$getThreshold(), pos, neg)))
      } else {
        return(list(prob = pmat))
      }
    }
  )
)
mlr_learners$add("classif.HCWB", LearnerClassifHCWB)


#suppressMessages(library(R6))
#suppressMessages(library(mlr3))
#suppressMessages(library(paradox))
#suppressMessages(library(compboost))

#source("learner-src/learner-helper.R")

#task = tsk("spam")
#l = lrn("classif.HCWB", predict_type = "prob", oob_fraction = 0.3, use_stopper = TRUE, mstop = 2000, use_stopper_auc = TRUE,
  #additional_auc_task = task, learning_rate = 0.1, momentum = 0.1, eps_for_break = 0, patience = 2)

#l$train(task)

#l$predict(task)$score(msrs(c("classif.auc", "time_train")))
#tail(l$model$cboost$getLoggerData())

#r = c(l$model$cboost$getLoggerData()$val_auc, l$model$cboost_restart$getLoggerData()$val_auc)
#plot(x = seq_along(r), y = r, type = "l")

#suppressMessages(library(mlr3tuning))
#suppressMessages(library(mlrintermbo))
#suppressMessages(library(mlr3learners))
#suppressMessages(library(mlr3extralearners))
#suppressMessages(library(mlr3pipelines))


#lr1 = lrn("classif.compboost", optimizer = "nesterov", use_stopper = TRUE,
  #eps_for_break = 0, patience = 2, oob_fraction = 0.3, predict_type = "prob",
  #mstop = 5000L, restart = TRUE, stop_both = TRUE, df_autoselect = TRUE,
  #oob_seed = 100)

#lr1$train(tsk("sonar"))

#length(lr1$model$cboost$getSelectedBaselearner())
#length(lr1$model$cboost_restart$getSelectedBaselearner())
#gridExtra::grid.arrange(
  #lr1$model$cboost$plotInbagVsOobRisk() + ggplot2::ylim(0, 1),
  #lr1$model$cboost_restart$plotInbagVsOobRisk() + ggplot2::ylim(0, 1),
  #ncol = 2)

#design = benchmark_grid(
  #tasks = tsk("sonar"),
  #learners = lr1,
  #resamplings = rsmp("cv", folds = 3)
#)

#bmr = benchmark(design, store_models = TRUE)
#bmr$aggregate(msrs(c("classif.auc", "classif.ce")))



