#' Get linear predictor from B-spline.
#'
#' @param x [numeric] Vector of x values
#' @param bs_dim [integer(1)] Number of base functions for the spline (default = 10L). (Corresponds to number of inner knots for the spline).
#' @param sigma [numeric(1)] Standard deviation for the normally distributed random variable from which the parameter are drawn (default = 3).
#' @param offset [numeric(1)] Shift on the y-axis of the linear predictor (default = 0).
#' @return The sum of \code{x} and \code{y}.
simSplines = function(x, bs_dim = 10L, sigma = 3, offset = 0, ...) {
  checkmate::assertNumeric(x = sigma, len = 1L)
  checkmate::assertNumeric(x = offset, len = 1L)
  if (bs_dim < 7) stop("Need bs_dim >= 7 !")

  nk = bs_dim - 2

  xu = max(x)
  xl = min(x)

  xr = xu - xl
  xl = xl - xr * 0.001
  xu = xu + xr * 0.001

  dx = (xu - xl)/(nk - 1)
  kn = seq(xl - dx * 3, xu + dx * 3, length = nk + 4 + 2)

  # create the spline basis functions
  X = splines::spline.des(kn, x, 4, x * 0)$design

  # multiply with random coefficients to get random functions
  coefs = rnorm(bs_dim, sd = sigma)

  return(list(y = X %*% coefs + offset, x = x, X = X, offset = offset, coefs = coefs, knots = kn))
}


#' Simulate data set of size n x (p + pnoise)
#'
#' @param n [integer(1)] Number of observations.
#' @param p [integer(1)] Number of features that have an effect on the response.
#' @param pnoise [integer(1)] Number of noise features (that have no effect on the response).
#' @param sn_ratio [numeric(1)] Signal to noise ratio sd(noise) / sd(mod) (default = 0.1).
#' @param featSimulator [function] Function for the generation of the linear predictor and feature with effect (default = simSplines).
#' @return The sum of \code{x} and \code{y}.
simData = function (n, p, pnoise, sn_ratio = 0, featSimulator = simSplines, seed = sample(100000, 1), ...) {
  ll_simulator_out = list()
  ll_feats         = list()
  ll_linpred       = list()
  ll_noise         = list()

  for (i in seq_len(p)) {
    # Simulate x value range somewhere between [0, 200]
    set.seed(seed + i)
    xmin = runif(n = 1L, min = 0, max = 100)
    set.seed(seed + p + i)
    xmax = xmin + runif(n = 1L, min = 0, max = 100)

    # Simulate feature values as uniformly distributed in that range:
    #x = sort(runif(n = n, min = xmin, max = xmax))
    set.seed(abs(seed - i))
    x = runif(n = n, min = xmin, max = xmax)

    # Get values for the linear predictor using the feature simulator:
    fs = featSimulator(x, ...)

    # Write into lists:
    ll_simulator_out[[i]] = list(x = x, y = fs$y, fs = fs)
    ll_feats = c(ll_feats, list(x))
    ll_linpred = c(ll_linpred, list(fs$y))
  }

  # Simulate noise feature:
  for (i in seq_len(pnoise)) {
    ll_noise = c(ll_noise, list(rnorm(n)))
  }

  ll_df = list()

  # Get data frame from the "real" feature by cbinding them together:
  df_feats = do.call(data.frame, ll_feats)
  # Give reasonable names:
  colnames(df_feats) = paste0("x", seq_len(p))

  # Same for the noise feature:
  df_noise = do.call(data.frame, ll_noise)
  colnames(df_noise) = paste0("noise", seq_len(pnoise))

  # Sum up "individual" linear predictors of the features:
  target = rowSums(do.call(cbind, ll_linpred))

  # Calculate the sd of the noise given the signal-to-noise ratio:
  if (sn_ratio == 0) {
    target_sd = 0
  } else {
    target_sd = sd(target) / sn_ratio
  }
  set.seed(seed)
  target = target +  rnorm(n, 0, target_sd)

  # Get the final dataset with response (y), effects (x1, ..., xp), and noise features (noise1, noisepnoise):
  df_out = cbind(data.frame(y = target), df_feats, df_noise)

  return(list(data = df_out, sim_poly = ll_simulator_out))
}


#' Simulate categorical data set of size n x (p + pnoise)
#'
#' @param n [integer(1)] Number of observations.
#' @param p [integer(1)] Number of features that have an effect on the response.
#' @param pnoise [integer(1)] Number of noise features (that have no effect on the response).
#' @param sn_ratio [numeric(1)] Signal to noise ratio sd(noise) / sd(mod) (default = 0.1).
#' @param nclasses [integer(1)] Number of classes per feature.
#' @param ncnoise [integer(1)] Number of classes per feature with no effect.
#' @return The sum of \code{x} and \code{y}.
simCategoricalData = function (n, p, pnoise, sn_ratio = 0, nclasses, ncnoise, ...) {
  ll_simulator_out = list()
  ll_feats         = list()
  ll_linpred       = list()
  ll_noise         = list()

  feat_prefix = letters[seq_len(p)]
  cls_names = apply(expand.grid(LETTERS, LETTERS), 1, paste0, collapse = "")

  for (i in seq_len(p)) {

    cls = paste0(feat_prefix[i], sample(cls_names, nclasses))
    means = runif(nclasses, min = -10, max = 10)
    if (ncnoise > 0) {
      idx_zero = sample(seq_len(nclasses), ncnoise)
      means[idx_zero] = 0
    }

    feat = sample(cls, n, TRUE)
    feat = data.frame(cls = feat, value = vapply(X = feat, FUN = function (cl) means[which(cl == cls)],
      FUN.VALUE = numeric(1L)))

    # Write into lists:
    ll_simulator_out[[i]] = list(param = data.frame(cls, means))
    ll_feats = c(ll_feats, list(feat$cls))
    ll_linpred = c(ll_linpred, list(feat$value))
  }

  for (i in seq_len(pnoise)) {

    cls = paste0("nsf", feat_prefix[i], sample(cls_names, nclasses))
    feat = sample(cls, n, TRUE)

    # Write into lists:
    ll_noise = c(ll_noise, list(feat))
  }

  ll_df = list()

  # Get data frame from the "real" feature by cbinding them together:
  df_feats = do.call(data.frame, ll_feats)
  # Give reasonable names:
  colnames(df_feats) = paste0("x", seq_len(p))

  # Same for the noise feature:
  df_noise = do.call(data.frame, ll_noise)
  colnames(df_noise) = paste0("noise", seq_len(pnoise))

  # Sum up "individual" linear predictors of the features:
  target = rowSums(do.call(cbind, ll_linpred))

  # Calculate the sd of the noise given the signal-to-noise ratio:
  if (sn_ratio == 0) {
    target_sd = 0
  } else {
    target_sd = sd(target) / sn_ratio
  }
  target = target +  rnorm(n, 0, target_sd)

  # Get the final dataset with response (y), effects (x1, ..., xp), and noise features (noise1, noisepnoise):
  df_out = cbind(data.frame(y = target), df_feats, df_noise)

  return(list(data = df_out, cat_params = ll_simulator_out))
}


#' Check info in config data-frame
#'
#' @param df [data.frame()] Data frame with configurations for the data simulation.
#' @return Logical value indicating if all information are there.
checkBMData = function (df, silent = TRUE) {
  clnames_required = c("n", "p", "pnoise", "sn_ratio", "rep")
  if (! "data.frame" %in% class(df)) stop("df has to be a data.frame")
  clnames = colnames(df)
  clnames_are_there = clnames_required %in% clnames
  if ((! all(clnames_are_there)) && (! silent)) {
    msg = paste0("Data frame has no columns: ", paste(clnames_required[! clnames_are_there], collapse = ", "))
    stop(msg)
  }
  return(all(clnames_are_there))
}
