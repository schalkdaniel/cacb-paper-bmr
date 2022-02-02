getFeatEffectData = function (bm_extract, bl, truth = TRUE)
{
  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)

  #bl_tab = table(bm_extract$trace_binning)
  #bl = names(which.max(bl_tab))
  bl_nbr = as.numeric(gsub("\\D", "", bl))

  coefs_binning = bm_extract$coef_binning[[bl]]
  coefs_nobinning = bm_extract$coef_nobinning[[bl]]

  x = dat$data[[paste0("x", bl_nbr)]]
  y = dat$sim_poly[[bl_nbr]]$y

  knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
  basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

  pred_binning = basis %*% coefs_binning
  pred_nobinning = basis %*% coefs_nobinning

  if (truth) {
    out = data.frame(
      x = rep(x, 3),
      y = c(y, pred_binning, pred_nobinning),
      method = rep(c("truth", "binning", "nobinning"), each = length(x))
    )
  } else {
    out = data.frame(
      x = rep(x, 2),
      y = c(pred_binning, pred_nobinning),
      method = rep(c("binning", "nobinning"), each = length(x))
    )
  }
  return (out)
}

getOobRiskData = function (bm_extract)
{
  set.seed(bm_extract$data_seed)

  risk_binning = bm_extract$log_binning$oob
  risk_nobinning = bm_extract$log_nobinning$oob_risk

  out = data.frame(
    iter = c(seq_along(risk_binning), seq_along(risk_nobinning)),
    risk = c(risk_binning, risk_nobinning),
    method = rep(c("binning", "nobinning"), times = c(length(risk_binning), length(risk_nobinning)))
  )

  return (out)
}



#x = runif(1000, 10, 1000)
#y = sin(x)
#yn = y + 1

#f = approxfun(x = x, y = (y - yn)^2)
#integrate(f = f, upper = max(x), lower = min(x))

getBLMSE = function (bm_extract)
{
  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)

  bls_nob = unique(bm_extract$trace_nobinning)
  bls_b = unique(bm_extract$trace_binning)

  bls_nob = bls_nob[!grepl(x = bls_nob, pattern = "noise")]
  bls_b = bls_b[!grepl(x = bls_b, pattern = "noise")]

  diff_nob = numeric(length(bls_nob))
  diff_b = numeric(length(bls_b))

  k = 1
  for (bl in bls_nob) {
    bl_nbr = as.numeric(gsub("\\D", "", bl))

    coefs_nobinning = bm_extract$coef_nobinning[[bl]]

    x = dat$data[[paste0("x", bl_nbr)]]
    y = dat$sim_poly[[bl_nbr]]$y

    knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
    basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

    pred_nobinning = basis %*% coefs_nobinning

    f_nob = approxfun(x = x, y = (y - pred_nobinning)^2)
    diff_nob[k] = integrate(f = f_nob, upper = max(x), lower = min(x))$value

    k = k+1
  }

  k = 1
  for (bl in bls_nob) {
    bl_nbr = as.numeric(gsub("\\D", "", bl))

    coefs_binning = bm_extract$coef_binning[[bl]]

    x = dat$data[[paste0("x", bl_nbr)]]
    y = dat$sim_poly[[bl_nbr]]$y

    knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
    basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

    pred_binning = basis %*% coefs_binning

    f_b = approxfun(x = x, y = (y - pred_binning)^2)
    diff_b[k] = integrate(f = f_b, upper = max(x), lower = min(x))$value

    k = k+1
  }

  return (list(mean_nob = mean(diff_nob), mean_b = mean(diff_b)))
}

