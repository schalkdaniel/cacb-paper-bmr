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
