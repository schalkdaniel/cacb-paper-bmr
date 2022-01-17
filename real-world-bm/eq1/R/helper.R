#' Get the stopping iteration of risk traces dependin on
#' the epsilon used for early stopping and the patience.
#' @param clog Log from compboost
#' @param patience Patience parameter for early stopping
#' @param eps_for_break The lower bound for the improvement
#' @param vname Column name containing the validation risk
#' @param tname Column name of the test risk
#' @param minimize The direction of the risk (minimization or maximization)
getStopInfo = function(clog, patience = 10L, eps_for_break = 0.00001,
  vname = "val_auc", tname = "test_auc", minimize = TRUE) {

  dsign = 1
  if (minimize) dsign = -1

  rval     = 1 - clog[[vname]]
  rvaldiff = diff(rval) / rval[-length(rval)] * dsign
  p0 = 0
  for (i in seq_along(rvaldiff)) {
    if (rvaldiff[i] < eps_for_break)
      p0 = p0 + 1
    else
      p0 = 0

    if (p0 == patience) break
  }
  if (i < (nrow(clog) - 1))
    istop = i - (patience + 1)
  else
    istop = nrow(clog)

  return(data.frame(stop = istop, val_auc = clog[[vname]][istop], test_auc = clog[[tname]][istop],
    seconds = clog$seconds[istop], learner = clog$learner[1], task = clog$task[1], fold = clog$fold[1],
    val_acu_max = max(clog[[vname]]), test_auc_max = max(clog[[tname]]), transition = clog$transition[1]))
}

#' Transform the task ids to meaningful names
#' @param tid Task id
#' @param ts Task object (to extract n and p)
taskIDtoName = function(tid, ts = NULL) {
  sapply(tid, function(tn) {

    if (tn == "spam") tnn = "Spam"
    if (tn == "168908") tnn = "Christine"
    if (tn == "9977") tnn = "Namao"
    if (tn == "7592") tnn = "Adult"
    if (tn == "168335") tnn = "MiniBooNE"
    if (tn == "189866") tnn = "Albert"

    if (! is.null(ts)) {
      ts = TASKS[[tn]]
      return(paste0(tnn, "\nn: ", ts$nrow, ", p: ", length(ts$feature_names)))
    } else {
      return(tnn)
    }
  })
}

#' Short helper to extract the right n characters
#' @param x Vector of characters from which the right part
#'   should be extracted.
#' @param n Number of chars to extract.
substrRight = function(x, n = 1) substr(x, nchar(x) - n + 1, nchar(x))


