getAllFeatEffectData = function (bm_extract, ndata = 1000L, coef_names = character(0L)) {
  checkmate::assertCharacter(x = coef_names, min.len = 1L)
  if (any(! coef_names %in% names(bm_extract))) stop("coef_names needs to be in bm_extract")

  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)

  n = bm_extract$config$n
  checkmate::assertIntegerish(x = ndata, upper = n, len = 1L, null.ok = TRUE)

  if (is.null(ndata)) ndata = n
  dat_idx = as.integer(seq(1, n, len = ndata))

  feat = colnames(dat$data)[grepl(pattern = "x", x = colnames(dat$data))]
  bls = paste0(feat, "_spline")

  out = list()
  for(bl in bls) {
    bl_nbr = as.numeric(gsub("\\D", "", bl))

    x = dat$data[[paste0("x", bl_nbr)]][dat_idx]
    y = dat$sim_poly[[bl_nbr]]$y[dat_idx]

    df_temp = data.frame(x = x, truth = y)

    knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
    basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

    for (cn in coef_names) {
      params = bm_extract[[cn]]
      if (bl %in% names(params)) {
        param = params[[bl]]
        pred = basis %*% param
        df_pred = data.frame(pred)
      } else {
        df_pred = data.frame(rep(0, ndata))
      }
      colnames(df_pred) = cn
      df_temp = cbind(df_temp, df_pred, bl = bl)
    }
    out[[bl]] = df_temp
  }
  return (out)
}

getFeatureIME = function (x, truth, pred, loss = function (x,y) (x-y)^2) {
  e = try({
    f_b = approxfun(x = x, y = loss(truth,pred))
    int = integrate(f = f_b, upper = max(x), lower = min(x))$value
  }, silent = TRUE)
  if (class(e) == "try-error") return (NA) else return (e)
  return ()
}

getFeatEffectDataBinning = function (bm_extract, bl, truth = TRUE) {
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
  return(out)
}

getOobRiskData = function (bm_extract) {
  set.seed(bm_extract$data_seed)

  risk_binning = bm_extract$log_binning$oob
  risk_nobinning = bm_extract$log_nobinning$oob_risk

  out = data.frame(
    iter = c(seq_along(risk_binning), seq_along(risk_nobinning)),
    risk = c(risk_binning, risk_nobinning),
    method = rep(c("binning", "nobinning"), times = c(length(risk_binning), length(risk_nobinning)))
  )
  return(out)
}

getBLMSE = function (bm_extract) {
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
  return(list(mean_nob = mean(diff_nob), mean_b = mean(diff_b)))
}



plotBlearnerTraces = function (bl, value = 1, n_legend = 5L, iter_limit = NULL, show_labels = TRUE, show_last_point = FALSE) {
  nbl = length(bl)
  if (is.null(iter_limit)) iter_limit = nbl
  bl = as.factor(bl[seq_len(iter_limit)])
  df_plot  = data.frame(iters = seq_along(bl), blearner = bl, value = value)

  if (length(value) %in% c(1L, length(bl))) {
    checkmate::assertNumeric(value)
  } else {
    stop("Assertion on 'value' failed: Must have length 1 or ", length(bl), ".")
  }
  checkmate::assertCount(n_legend, positive = TRUE)

  # Aggregate value by calculating the cumulative sum grouped by base-learner:
  df_plot = do.call(rbind, lapply(X = levels(bl), FUN = function (lab) {
    df_temp = df_plot[df_plot$blearner == lab, ]
    df_temp = df_temp[order(df_temp$iters), ]
    df_temp$value = cumsum(df_temp$value) / nbl

    return(df_temp)
  }))

  # Get top 'n_legend' base-learner that are highlighted:
  top_values = vapply(X = levels(bl), FUN.VALUE = numeric(1L), FUN = function (lab) {
    df_temp = df_plot[df_plot$blearner == lab, ]
    return (max(df_temp$value))
  })
  top_labs = as.factor(names(sort(top_values, decreasing = TRUE)))[seq_len(n_legend)]

  idx_top_lab = df_plot$blearner %in% top_labs

  df_plot_top    = df_plot[idx_top_lab, ]
  df_plot_nottop = df_plot[! idx_top_lab, ]

  df_label = do.call(rbind, lapply(X = top_labs, FUN = function (lab) {
    df_temp = df_plot[df_plot$blearner == lab, ]
    df_temp[which.max(df_temp$iters), ]
  }))

  gg = ggplot2::ggplot() +
    ggplot2::geom_line(data = df_plot_top, ggplot2::aes(x = iters, y = value, color = blearner), show.legend = FALSE) +
    ggplot2::geom_line(data = df_plot_nottop, ggplot2::aes(x = iters, y = value, group = blearner), alpha = 0.2, show.legend = FALSE)

  if (show_last_point) {
    gg = gg + geom_point(data = df_plot_top %>% filter(iters == iter_limit), aes(x = iters, y = value, color = blearner), show.legend = FALSE) +
      geom_point(data = df_plot_nottop %>% filter(iters == iter_limit), aes(x = iters, y = value, group = blearner), alpha = 0.2,  show.legend = FALSE)
  }

  if (show_labels) {
    gg = gg + ggrepel::geom_label_repel(data = df_label, ggplot2::aes(x = iters, y = value, label = round(value, 4), fill = blearner),
      colour = "white", fontface = "bold", show.legend = TRUE)
  }
  gg = gg + ggplot2::xlab("Iteration") +
    ggplot2::ylab("Cumulated Value\nof Included Base-Learner") +
    ggplot2::scale_fill_discrete(name = paste0("Top ", n_legend, " Base-Learner")) +
    ggplot2::guides(color = FALSE) +
    ggplot2::xlim(0, nbl)

  return(gg)
}


transformBinaryToParam = function (binary_params) {
  pure = binary_params
  pnames = names(pure)
  feats = unique(vapply(pnames, FUN.VALUE = character(1L), FUN = function (pn) strsplit(pn, split = "_")[[1]][1]))
  out = lapply(feats, function (feat) {
    idx_feat = grepl(pattern = feat, x = pnames)
    tmp = pure[idx_feat]
    out = unlist(tmp)
    names(out) = NULL
    out_names = vapply(names(tmp), FUN.VALUE = character(1L), FUN = function (pn) strsplit(pn, split = "_")[[1]][3])
    names(out_names) = NULL
    return (data.frame(cls = out_names, means = out))
  })
  names(out) = feats
  return(out)
}

transformRidgeToParam = function (est_params, data) {
  nms = names(est_params)
  out = lapply(nms, function (pm) {
    if (grepl(pattern = "noise", x = pm)) {
      return (data.frame(cls = paste0("dummy", seq_along(est_params[[pm]])), means = est_params[[pm]]))
    } else {
      x = data[[substr(pm, 2, nchar(pm))]]
      lvls = unique(x)
      return (data.frame(cls = lvls, means = est_params[[pm]]))
    }
  })
  names(out) = nms
  return(out)
}

getNoiseMSE = function (real_params, est_params, include_noise = TRUE, just_noise = FALSE) {
  names(real_params) = paste0("xx", seq_along(real_params))
  out = lapply(names(est_params), function (pn) {
    params = est_params[[pn]]

    if (grepl(pattern = "noise", pn)) {
       if (include_noise) {
         mout = mean(params[,2]^2)
         attr(mout, "n.not.sel") = NA
       } else {
         mout = NA
         attr(mout, "n.not.sel") = NA
       }
     } else {
      real = real_params[[pn]]
      idx_zeros = real$param$means == 0
      tmp = dplyr::left_join(real$param, params, by = "cls")
      if (any(is.na(tmp$means.y))) { tmp$means.y = ifelse(is.na(tmp$means.y), 0, tmp$means.y)}
      out = (tmp$means.x - tmp$means.y)^2

      if (just_noise) {
        mout = mean(out[idx_zeros], na.rm = TRUE)
      } else {
        if (include_noise) {
          mout = mean(out, na.rm = TRUE)
        } else {
          mout = mean(out[!idx_zeros], na.rm = TRUE)
        }
      }

      if (is.nan(mout)) mout = 0
      attr(mout, "n.not.sel") = sum(out[idx_zeros] == 0)
      attr(mout, "n.wrong.not.sel") = sum(tmp$means.x[tmp$means.y==0] != 0)
    }
    return (mout)
  })
  names(out) = names(est_params)
  mout = mean(unlist(out), na.rm = TRUE)
  n_not_sel = mean(unlist(lapply(out, function(m) attr(m, "n.not.sel"))), na.rm = TRUE)
  n_wrong_not_sel = mean(unlist(lapply(out, function(m) attr(m, "n.wrong.not.sel"))), na.rm = TRUE)
  return(list(mean = mout, n_not_sel = n_not_sel, n_wrong_not_sel = n_wrong_not_sel))
}


getCategoricalMSE = function (real_params, est_params, include_noise = TRUE) {
  names(real_params) = paste0("xx", seq_along(real_params))
  out = lapply(names(est_params), function (pn) {
    params = est_params[[pn]]

    if (grepl(pattern = "noise", pn)) {
       if (include_noise) {
         mout = mean(params[,2]^2)
         attr(mout, "n.not.sel") = NA
       } else {
         mout = NA
         attr(mout, "n.not.sel") = NA
       }
     } else {
      real = real_params[[pn]]
      tmp = dplyr::left_join(real$param, params, by = "cls")
      out = (tmp$means.x - tmp$means.y)^2
      if (any(is.na(out))) out[is.na(out)] = tmp$means.x[is.na(out)]^2
      mout = mean(out, na.rm = TRUE)
      if (is.nan(mout)) mout = mean(tmp$mean.x^2)
    }
    return (mout)
  })
  names(out) = names(est_params)
  mout = mean(unlist(out), na.rm = TRUE)
  return(mout)
}
