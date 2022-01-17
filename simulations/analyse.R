## ============================================================== ##
##
##                              SETUP
##
## ============================================================== ##

library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(gridExtra)
library(ggsci)

# Set variable if ggplots should be saved:
SAVEGG = FALSE

BASE_DIR = here::here("simulations/")
FIG_DIR  = function(fig_name) paste0(BASE_DIR, "figures/", fig_name)
RES_DIR  = function(res_name) paste0(BASE_DIR, "/results-summarized/", res_name)

# Load helper functions:
source(paste0(BASE_DIR, "R/bm-sim-data.R"))
source(paste0(BASE_DIR, "R/helper.R"))

# Set theme:
theme_set(
  theme_minimal() +
  #theme_minimal(base_family = font) +
  ggplot2::theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", face = "bold", size = 10),
    #axis.text = element_text(size = 9),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    #legend.title = element_text(size = 9),
    #legend.text = element_text(size = 7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
)
MY_COLOR = scale_color_uchicago()
MY_FILL = scale_fill_uchicago()

# Gained from \printinunitsof{cm}\prntlen{\textwidth} in the latex doc
DINA4WIDTH = 162
MYCOLORS = ggsci::pal_uchicago()(6)[4:6]

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend = function(a.gplot) {
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)
}



## ============================================================== ##
##
##                         VISUALIZATIONS
##
## ============================================================== ##

## Figure 1:
## =====================================

# Data:
# - `df_plt` data.frame
load(RES_DIR("fig-agbm-restart-iters.Rda"))

agbm_iters = which.min(df_plt[df_plt$method == "ACWB", "Risk"])
iter = 3000L

df_plt$method[df_plt$method == "ACWB with restart"] = "hCWB"

df_plt$method[df_plt$method == "hCWB"] = "HCWB"
df_plt$method = factor(df_plt$method, levels = c("CWB", "ACWB", "HCWB"))

# Plot
gg = df_plt %>% filter(type == "oob", Iteration <= iter) %>%
  ggplot(aes(x = Iteration, y = Risk, color = method)) +
  geom_vline(xintercept = agbm_iters, color = "dark red", alpha = 0.5,
    linetype = "dashed", size = 0.7) +
  geom_line(size = 0.9) +
  xlim(0, iter) +
  xlab("Iterations") +
  ylab("Validation loss") +
  labs(color = "") +
  annotate("text", x = agbm_iters * 1.1, y = max(df_plt$Risk),
    label = "Optimal stopping\nACWB", color = "dark red",
    hjust = 0, vjust = 1,  size = 2.7) +
  scale_fill_manual(values = MYCOLORS) +
  scale_color_manual(values = MYCOLORS)

if (SAVEGG) {
  ggsave(
    plot = gg,
    filename = FIG_DIR("fig-optim-emp-risk.pdf"),
    width = DINA4WIDTH*0.5,
    height = DINA4WIDTH*0.25,
    units = "mm")
}



## H1: Figure 2
## =====================================

# Data:
# - `df_plt_mem` data.frame
load(RES_DIR("fig-binning-memory.Rda"))
# - `df_plt_run` data.frame
load(RES_DIR("fig-binning-runtime.Rda"))

# Filter number of features (adding all #features
# makes the plot messy):
df_plt_mem = df_plt_mem %>%
  filter(ptotal %in% c(7, 10, 15, 30, 60, 100, 150, 300))

## MEMORY
gg1 = ggplot(
    data = df_plt_mem,
    aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_hline(
    yintercept = 1,
    color = "dark red",
    lty = 2) +
  #geom_line() +
  geom_smooth(se = FALSE, alpha = 0.7, size = 0.5) +
  geom_point(size = 2, alpha = 0.7) +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  scale_y_continuous(breaks = c(1, 2, 4, 6)) +
  xlab("Number of rows\n(log scale)") +
  ylab("Memory savings  ") +
  labs(color = "Number of\nfeatures") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Equal memory",
    color = "dark red",
    vjust = -0.5,
    hjust = 0.9,
    size = 3) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5), legend.position = "bottom") +
  MY_COLOR + MY_FILL +
  guides(color = guide_legend(nrow = 2))

## RUNTIME:
df_plt_runm = df_plt_run %>%
  filter(ptotal %in% c(7,10,15,30,60,100,150,300))

df_plt_run = df_plt_run %>%
  filter(rel_time < 10, rel_time > 2, phase == "Fitting") %>%
  group_by(nrows, ptotal) %>%
  summarize(med = median(rel_time), min = min(rel_time), max = max(rel_time))

dodge_width = 0.25

gg2 = ggplot() +
  geom_hline(
    yintercept = 1,
    lty = 2,
    col = "dark red") +
  geom_point(
    data = df_plt_run,
    aes(x = nrows, y = med, color = as.factor(ptotal)),
    size = 2,
    alpha = 0.7,
    position = position_dodge(width = dodge_width),
    show.legend = FALSE) +
  geom_errorbar(
    data = df_plt_run,
    aes(x = nrows, ymax = max, ymin = min, color = as.factor(ptotal)),
    na.rm = TRUE,
    position = position_dodge(width = dodge_width),
    size = 0.5,
    show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5)) +
  xlab("Number of rows\n(log scale)") +
  ylab("Speedup") +
  labs(color = "Number of\nfeatures", fill = "Number of\nFeatures") +
  scale_y_continuous(breaks = c(1, 2, 4, 6)) +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Equal runtime",
    color = "dark red",
    vjust = -0.5,
    hjust = 0.7,
    size = 3) +
  MY_COLOR + MY_FILL

# Extract legend of first ggplot:
mylegend = g_legend(gg1)

p3 = grid.arrange(
  arrangeGrob(
    gg1 +
      theme(legend.position = "none"),
    gg2 +
      theme(legend.position = "none"),
    nrow = 1),
  mylegend,
  nrow = 2,
  heights = c(7, 2))

dev.off()

if (SAVEGG) {
  ggsave(
    plot = p3,
    filename = FIG_DIR("fig-binning-runtime-memory.pdf"),
    width = DINA4WIDTH * 0.9,
    height = DINA4WIDTH * 0.4,
    units = "mm")
}




## H2: MISE vizualization + MISE binning
## ===========================================

# Variables for the example simulation/visualization:
seed = 31415

n = 100000
p = 4
pnoise = 2
sn_ratio = 0.1


# Simulate data for the example visualization:

set.seed(seed)
dat = simData(n, p, pnoise)
dat_noise = dat$data

set.seed(seed)
dat_noise$y = rnorm(n = n, mean = dat_noise$y, sd = sd(dat_noise$y) / sn_ratio)

# remotes::install_github("schalkdaniel/compboost", ref = "c68e8fb32aea862750991260d243cdca1d3ebd0e")
# Be careful with BH version > 1.45, it does not work with the version of compboost at the
# statet commit.
library(compboost)

# Train two models to compare the estimated feature effects:

set.seed(seed)
cboost = boostSplines(data = dat_noise, target = "y", iterations = 10000L, learning_rate = 0.01,
  loss = LossQuadratic$new(), stop_args = list(eps_for_break = 0, patience = 3L), oob_fraction = 0.3,
  df = 7)

set.seed(seed)
cboost_bin = boostSplines(data = dat_noise, target = "y", iterations = 10000L, learning_rate = 0.01,
  loss = LossQuadratic$new(), stop_args = list(eps_for_break = 0, patience = 3L), oob_fraction = 0.3,
  bin_root = 2, df = 7)

# MISE is 49.08 for cwb
#         49.10 for cwb with binning
# May differ slightly, depending on RNG and seeding options.
if (FALSE) {
  mise = numeric(4L)
  mise_bin = numeric(4L)
  for (i in seq_len(4)) {
    y = dat$sim_poly[[i]]$y
    x = dat$data[[paste0("x", i)]]

    knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
    basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

    pars = cboost$getEstimatedCoef()[[paste0("x", i, "_spline")]]
    pars_bin = cboost_bin$getEstimatedCoef()[[paste0("x", i, "_spline")]]

    bl_pred = basis %*% pars
    bl_pred_bin = basis %*% pars_bin

    mise[i] = getFeatureIME(x = x, truth = y, pred = bl_pred)
    mise_bin[i] = getFeatureIME(x = x, truth = y, pred = bl_pred_bin)
  }
  mean(mise)
  mean(mise_bin)
}

# Use 1000 points for plotting:
ndata = 1000L
dat_idx = as.integer(seq(1, n, len = ndata))

feat = colnames(dat$data)[grepl(pattern = "x", x = colnames(dat$data))]
bls = paste0(feat, "_spline")
coef_names = c("coef_binning", "coef_nobinning")
coefs = list(coef_binning = cboost_bin$getEstimatedCoef(), coef_nobinning = cboost$getEstimatedCoef())

# Generate feature effects depending on the parameter estimates:
out = list()
for (bl in bls) {
  bl_nbr = as.numeric(gsub("\\D", "", bl))

  x = dat$data[[paste0("x", bl_nbr)]][dat_idx]
  y = dat$sim_poly[[bl_nbr]]$y[dat_idx]

  df_temp = data.frame(x = x, truth = y)

  knots = compboostSplines::createKnots(values = x, n_knots = 20, degree = 3)
  basis = compboostSplines::createSplineBasis(values = x, degree = 3, knots = knots)

  for (cn in coef_names) {
    params = coefs[[cn]]
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

# Generate data frame with partial effects data:
ll_fe = lapply(out, function(df) {
  df %>%
    pivot_longer(cols = all_of(c(coef_names, "truth")), names_to = "method", values_to = "effect") %>%
    group_by(method) %>%
    mutate(y = effect - mean(effect)) %>%
    arrange(method, x)
})
df_fe = do.call(rbind, ll_fe)

# Add categories for different lines:
feat_id = as.integer(gsub("\\D", "", df_fe$bl))
feat = paste0("Feature ", feat_id)
df_fe$feat = factor(feat, levels = paste0("Feature ", sort(unique(feat_id))))

df_fe$line = df_fe$method
df_fe$line[df_fe$line == "truth"] = "Truth"
df_fe$line[df_fe$line == "coef_binning"] = "Binning"
df_fe$line[df_fe$line == "coef_nobinning"] = "No Binning"

df_fe$line = factor(df_fe$line)
df_fe$line = ordered(df_fe$line, c("Truth", "No Binning", "Binning"))
df_fe$linetype = ifelse(df_fe$line == "Binning", "solid", "dashed")

df_area = cbind(
  df_fe %>% select(x, bl, method, y, feat, line) %>% filter(line == "No Binning"),
  df_fe %>% ungroup() %>% mutate(y_t = y, line_t = line) %>% select(y_t, line_t) %>% filter(line_t == "Truth"))

gg1 = ggplot() +
  geom_ribbon(
    data = df_area,
    aes(ymin = y, ymax = y_t, x = x),
    fill = "dark red",
    alpha = 0.2) +
  geom_line(
    data = df_fe,
    aes(x = x, y = y, color = line, linetype = linetype)) +
  xlab("x") +
  ylab("Partial effect") +
  labs(color = "", linetype = "") +
  scale_linetype(guide = "none") +
  MY_COLOR + MY_FILL +
  facet_wrap(. ~ feat, scales = "free") + #, scales = "free")
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))


# Data:
# - `df_imse` data.frame
load(RES_DIR("fig-binning-imse.Rda"))

df_imse$method_n = as.factor(df_imse$method_n)
levels(df_imse$method_n) = c("Binning", "Binning 4", "No binning")

gg2 = ggplot(
    data = df_imse %>% filter(method_n != "Binning 4"),
    aes(x = as.factor(nrows), y = mimse, fill = method_n, color = method_n)) +
  geom_boxplot(alpha = 0.2, lwd = 0.4) +
  xlab("Number of rows (log scale)") +
  ylab("MISE") +
  labs(color = "", fill = "") +
  MY_COLOR + MY_FILL +
  facet_grid(paste0("SNR = ", sn_ratio) ~ ., scales = "free_y") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 20, hjust = 1))

if (SAVEGG) {
  ggsave(
    plot = grid.arrange(gg1, gg2, ncol = 2),
    filename = FIG_DIR("fig-binning-imse.pdf"),
    width = DINA4WIDTH,
    height = DINA4WIDTH * 0.58,
    units = "mm")
}




## H3: ACWB/hCWB MISE + iters
## ==============================================

## Data:
load(RES_DIR("res-aggregated.Rda"))

# Use 10 categories for plotting:
ncat = 10L
breaks = c(1.3^(-seq(8, 49, length.out = ncat)), 0)
breaks[1] = 0.1

df_agbm = df_agbm %>%
  mutate(mom_cat = cut(momentum, breaks = breaks))

levels(df_agbm$mom_cat) = c(levels(df_agbm$mom_cat), "COD")
df_agbm$mom_cat[df_agbm$method == "COD"] = "COD"

pretty_levels = paste0("1.3e-", seq(8, 49, length.out = ncat))

# Create new data frame for plotting:
df_plt = df_agbm %>%
  filter(momentum > 0.0003) %>%
  group_by(seed, rep, momentum) %>%
  summarise(
    n = n(),
    mise = mise[method == "COD"] - mise,
    mse_oob = mse_oob[method == "COD"] - mse_oob,
    time = time_fit,
    iters = iters[method == "COD"] / iters,
    iterso = iters,
    mom_cat = mom_cat,
    method = method,
    rows = rows,
    cols = cols,
    pnoise = pnoise,
    pnoise_rel = ifelse(pnoise / cols == 0.4, 0.5, pnoise / cols),
    snr = snr,
    method = ifelse(method == "COD", "COD", ifelse(method == "AGBM", "ACWB", "hCWB")))

qlower = 0.25
qupper = 0.75
df_tmp = df_plt %>%
  filter(method != "COD") %>%
  group_by(method, mom_cat, snr) %>%
  summarize(
    med_mise = median(mise), lower_mise = quantile(mise, qlower), upper_mise = quantile(mise, qupper),
    med_mse = median(mse_oob), lower_mse = quantile(mse_oob, qlower), upper_mse = quantile(mse_oob, qupper),
    med_iters = median(iters), lower_iters = quantile(iters, qlower), upper_iters = quantile(iters, qupper)
  )

gg1 = df_tmp %>% na.omit() %>% mutate(method = ifelse(method == "ACWB", "ACWB", "HCWB")) %>%
  ggplot(aes(color = mom_cat)) +
    geom_vline(xintercept = 1, linetype = "dashed", color = "dark red", alpha = 0.3, size = 0.2) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "dark red", alpha = 0.3,  size = 0.2) +
    geom_point(aes(x = med_iters, y = med_mise), size = 0.6) +
    geom_errorbar(aes(x = med_iters, ymin = lower_mise, ymax = upper_mise), alpha = 0.8, size = 0.4) +
    geom_errorbarh(aes(xmin = lower_iters, xmax = upper_iters, y = med_mise), alpha = 0.8, size = 0.4) +
    MY_COLOR +
    MY_FILL +
    xlab("Speedup") +
    ylab("MISE difference") +
    labs(color = "Momentum", fill = "Momentum") +
    scale_x_continuous(trans = "log2") +
    theme(strip.text = element_text(color = "black", face = "bold", size = 8),
      axis.text.x = element_text(size = 10, angle = 50, hjust = 1),
      axis.text.y = element_text(size = 10)) +
    facet_grid(paste0("SNR = ", snr) ~ method, scales = "free")

if (SAVEGG) {
  ggsave(
    plot = gg1,
    filename = FIG_DIR("fig-acwb-iters-mise.pdf"),
    width = DINA4WIDTH*0.7,
    height = DINA4WIDTH*0.43,
    units = "mm")
}




## H4
## =====================================

## Appendix: Empirical computational complexity
## ------------------------------------------------

# Data:
load(RES_DIR("fig-comp-complexity.Rda"))

ll_res = list()
ll_est = list()
ll_res_bin = list()
ll_est_bin = list()

for (nrow0 in unique(df_binning_runtime$nrows)) {
  df_cwb = df_binning_runtime %>%
    filter(method == "nobinning", nrows == nrow0) %>%
    mutate(time = time_fit + time_init, ptotal = ncols + ncolsnoise)

  df_bin = df_binning_runtime %>%
    filter(method == "binning", nrows == nrow0) %>%
    mutate(time = time_fit + time_init, ptotal = ncols + ncolsnoise)

  # CWB: $\mathcal{O}(J(\bd^2n + \bd^3) + MJ(\bd^2 + dn))$
  ffun = function (pars, dat) {
    return(with(dat, (ptotal^pars["a0"] * (20^pars["a"]*nrows^pars["c0"] + 20^pars["b"]) +
      200^pars["b0"] * ptotal^pars["a1"] *(20^pars["d"] + 20^pars["e"] * nrows^pars["c1"])) / pars["v"]))
  }
  # Binning: $\mathcal{O}(J(\bd^2n^\ast  + n + \bd^3) + MJ(\bd^2 + \bd n^\ast + n))$
  ffun_bin = function (pars, dat) {
    return(with(dat, (ptotal^pars["a0"] * (20^pars["a"]*sqrt(nrows)^pars["b0"] + nrows^pars["b1"] + 20^pars["b"]) +
      200^pars["c0"] * ptotal^pars["a1"] * (20^pars["d"] + 20^pars["e"] * sqrt(nrows)^pars["b2"] + nrows^pars["b3"])) / pars["v"]))
  }
  fopt = function (pars, dat, target, fun) {
    y = fun(pars, dat)
    y0 = dat[[target]]
    return (sum((y - y0)^2))
  }
  diffs = ffun(c(a = 2, b = 3, d = 2, e = 1, v = 1, a0 = 1, a1 = 1, b0 = 1, c0 = 1, c1 = 1), df_cwb) / df_cwb[["time"]]
  diffs_bin = ffun_bin(c(a = 2, b = 3, d = 2, e = 1, v = 1, a0 = 1, a1 = 1, b0 = 1, b1 = 1, b2 = 1, b3 = 1, c0 = 1), df_bin) / df_bin[["time"]]

  par_start = c(a = 2, b = 3, d = 2, e = 1, v = mean(diffs), a0 = 1, a1 = 1, b0 = 1, c0 = 1, c1 = 1)
  par_start_bin = c(a = 2, b = 3, d = 2, e = 1, v = mean(diffs_bin), a0 = 1, a1 = 1, b0 = 1, b1 = 1, b2 = 1, b3 = 1, c0 = 1)

  set.seed(31415)
  est = optim(par = par_start, fn = fopt, dat = df_cwb, target = "time", fun = ffun,  method = "Nelder-Mead")
  set.seed(31415)
  est_bin = optim(par = par_start_bin, fn = fopt, dat = df_bin, target = "time", fun = ffun_bin,  method = "Nelder-Mead")

  fit = ffun(est$par, df_cwb)
  fit_bin = ffun_bin(est_bin$par, df_bin)

  ll_res[[as.character(nrow0)]] = data.frame(d = rep(df_cwb$ptotal, 2), y = c(df_cwb$time, fit),
    what = rep(c("Time", "Estimate"), each = length(fit)), nrow = nrow0)
  ll_est[[as.character(nrow0)]] = est
  ll_res_bin[[as.character(nrow0)]] = data.frame(d = rep(df_bin$ptotal, 2), y = c(df_bin$time, fit_bin),
    what = rep(c("Time", "Estimate"), each = length(fit_bin)), nrow = nrow0)
  ll_est_bin[[as.character(nrow0)]] = est_bin
}

pars_cwb = rowMeans(sapply(ll_est, function (x) x$par))
pars_cwb
pars_bin = rowMeans(sapply(ll_est_bin, function (x) x$par))
pars_bin

### R spared:
getRSQ = function (ll_res) {
  df_res = do.call(rbind, ll_res)
  y = df_res[df_res$what == "Time","y"]
  yhat = df_res[df_res$what == "Estimate","y"]
  sum((yhat - mean(y))^2) / sum((y - mean(y))^2)
}
getRSQ(ll_res)
getRSQ(ll_res_bin)


df_plt = do.call(rbind, ll_res)
df_plt0 = df_plt  %>%
  filter(what == "Time") %>%
  group_by(d, nrow) %>%
  summarize(med = median(y), upper = quantile(y, 0.99), lower = quantile(y, 0.01))

x = 7:300
nrow = c(5000, 10000, 20000, 50000, 100000)
df_line = do.call(rbind, lapply(nrow, function (n) {
  tmp = data.frame(nrows = n, ptotal = x)
  tmp$y = ffun(ll_est[[as.character(n)]]$par, tmp)
  tmp
}))

gg_cwb = ggplot() +
  geom_point(data = df_plt[round(seq(1,9600,len=500)),] %>% na.omit(), aes(x = d, y = y, color = as.factor(nrow)), stroke = 0.5, size = 1.8,
    alpha = 0.2) + #, shape = 4) +
  geom_line(data = df_line, aes(x = ptotal, y = y, color = as.factor(nrows)), size = 0.7) +
  scale_x_continuous(trans = "log10") +
  labs(color = "n") +
  xlab("d (log scale)") +
  ylab("Time in seconds") +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1)) +
  MY_COLOR + MY_FILL

df_plt_bin = do.call(rbind, ll_res_bin)
df_plt_bin0 = df_plt_bin  %>%
  filter(what == "Time") %>%
  group_by(d, nrow) %>%
  summarize(med = median(y), upper = quantile(y, 0.99), lower = quantile(y, 0.01))

df_line_bin = do.call(rbind, lapply(nrow, function (n) {
  tmp = data.frame(nrows = n, ptotal = x)
  tmp$y = ffun_bin(ll_est_bin[[as.character(n)]]$par, tmp)
  tmp
}))

gg_bin = ggplot() +
  geom_point(data = df_plt_bin[round(seq(1,9600,len=500)),], aes(x = d, y = y, color = as.factor(nrow)),
    stroke = 0.5, size = 1.8, alpha = 0.2) + #, shape = 4) +
  geom_line(data = df_line_bin, aes(x = ptotal, y = y, color = as.factor(nrows)), size = 0.7) +
  scale_x_continuous(trans = "log10") +
  labs(color = "n") +
  xlab("d (log scale)") +
  ylab("Time in seconds") +
  MY_COLOR + MY_FILL

mylegend = g_legend(gg_cwb)

p3 = grid.arrange(
  arrangeGrob(
    gg_cwb +
      theme(legend.position="none") +
      #ylim(0, 3500) +
      ggtitle("Computational complexity", "CWB"),
    gg_bin +
      theme(legend.position="none") +
      #ylim(0, 3500) +
      ggtitle("", "CWB with binning"),
    nrow=1),
  mylegend,
  nrow = 2,
  heights = c(9, 2))
dev.off()

if (SAVEGG) {
  ggsave(
    plot = p3,
    filename = FIG_DIR("fig-comp-complexity.pdf"),
    width = DINA4WIDTH * 0.9,
    height = DINA4WIDTH * 0.4,
    units = "mm")
}





## Computational complexity ACWB:
## --------------------------------------------

# Data:
load(RES_DIR("df_comp_acwb.Rda"))

## Theoretical formula: K * (d^2 * n+d^3) + 2 * M * K(d^2 + d * n)
compACWB = function(pars, n, p, iters, npars) {
  comp = p^pars[1] * (npars^pars[2] * n^pars[3] + npars^pars[4]) + 2 * iters^pars[5] * p^pars[6] * (npars^pars[7] + npars^pars[8] * n^pars[9])
  return(comp / pars[10])
}

compACWBd = function(pars = c(1, 2, 1, 3, 1, 1, 2, 1, 1, 1), dat) {
  compACWB(pars, dat$nrows, dat$ptotal, 200, 20)
}

fopt = function(pars, dat, loss = function(y, y0) abs(y - y0)) {
  y  = compACWBd(pars, dat)
  y0 = dat$time
  return(mean(loss(y, y0)))
}

diffs = compACWBd(dat = df_comp_acwb) / df_comp_acwb$time
par_start = c(1, 2, 1, 3, 1, 1, 2, 1, 1, mean(diffs))
est = optim(par = par_start, fn = fopt, dat = df_comp_acwb)
#est = optim(par = par_start, fn = fopt, dat = df_comp_acwb, loss = function(y, y0) (y - y0)^2)

cbind(Estimates = est$par, Startvalues = par_start, Difference = est$par - par_start)

time_est = compACWBd(pars = est$par, dat = df_comp_acwb)


df_tmp1 = df_comp_acwb %>%
  group_by(nrows, ptotal) %>%
  summarize(time = mean(time))

df_tmp2 = df_comp_acwb %>% cbind(time_est = time_est)

### R^2
1 - sum((df_tmp2$time_est - df_tmp2$time)^2) / sum((df_tmp2$time - mean(df_tmp2$time))^2)

set.seed(31415)
sel_idx = sample(seq_len(60L), 10)

ptotal_selection = c(10, 20, 40, 100, 150, 300)
df_plt = df_tmp2 %>%
  filter(ptotal %in% ptotal_selection) %>%
  group_by(nrows, ptotal) %>%
  summarize(time = time[sel_idx], time_est = time_est[sel_idx])

gg_comp = ggplot(df_plt) +
  geom_point(aes(x = nrows, y = time / 60, color = as.factor(ptotal)), alpha = 0.2) +
  geom_line(aes(x = nrows, y = time_est / 60, color = as.factor(ptotal))) +
  xlab("n") +
  ylab("Runtime\n(in minutes)") +
  labs(color = "K") +
  theme(legend.key.size = unit(0.6, 'lines')) +
  ggsci::scale_color_uchicago()

if (SAVEGG) {
  ggsave(gg_comp,
    filename = FIG_DIR("fig-h4.pdf"),
    width = DINA4WIDTH * 0.7,
    height = DINA4WIDTH * 0.25,
    units = "mm")
}





## APPENDIX:
## =======================================

if (FALSE) {


## ACWB memory + runtime:
## =====================================

## MEMORY:
load(RES_DIR("fig-acwb-mem.Rda"))

gg = ggplot(
    data = df_plt_mem %>% filter(ptotal %in% c(10, 30, 75, 100, 150, 300)),
    aes(x = nrows, y = rel_mem, color = as.factor(ptotal), group = paste0(ncols, ncolsnoise))) +
  geom_hline(
    yintercept = 1,
    color = "dark red",
    lty = 2) +
  #geom_line() +
  geom_smooth(se = FALSE, alpha = 0.7) +
  geom_point(size = 10, alpha = 0.7) +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  MY_COLOR +
  MY_FILL +
  xlab("Number of Rows\n(log10 Scale)") +
  ylab(expression(atop("Memory improvement\n", paste(Mem["CWB"], "/", Mem["ACWB"], sep = "")))) +
  labs(color = "Number of\nFeatures") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Baseline (used memory is equal)",
    color = "dark red",
    vjust = 1.5,
    hjust = 1,
    size = 1.5)

if (FALSE) {
  ggsave(
    plot = gg,
    filename = FIG_DIR("fig-acwb-memory.pdf"),
    width = DINA4WIDTH ,
    height = DINA4WIDTH * 0.7,
    units = "mm")
}


## RUNTIME:

load(RES_DIR("fig-acwb-run.Rda"))

df_plt_run = df_plt_run %>%
  filter(rel_time < 0.7) %>%
  group_by(nrows, ptotal) %>%
  summarize(med = median(rel_time), min = min(rel_time), max = max(rel_time)) %>%
  filter(ptotal %in% c(10, 30, 75, 100, 150, 300))

dodge_width = 0.25

gg = ggplot() +
  geom_hline(
    yintercept = 1,
    lty = 2,
    col = "dark red") +
  geom_point(
    data = df_plt_run,
    aes(x = nrows, y = med, color = as.factor(ptotal)),
    size = 4,
    alpha = 0.7,
    position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    data = df_plt_run,
    aes(x = nrows, ymax = max, ymin = min, color = as.factor(ptotal)),
    na.rm = TRUE,
    position = position_dodge(width = dodge_width),
    size = 1.3) +
  MY_COLOR + MY_FILL +
  xlab("Number of Rows\n(log10 Scale)") +
  ylab(expression(atop("Speedup\n", paste(Time["No Binning"], "/", Time["Binning"], sep = "")))) +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures") +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  annotate("text",
    x = max(df_plt_mem$nrows),
    y = 1,
    label = "Baseline (runtime is equal)",
    vjust = 1.5,
    color = "dark red",
    hjust = 1,
    size = 1.5) +
  ylim(0.25, 1)

if (FALSE) {
  ggsave(
    plot = gg,
    filename = FIG_DIR("fig-acwb-runtime.pdf"),
    width = DINA4WIDTH,
    height = DINA4WIDTH * 0.7,
    units = "mm")
}


## Categorical features:
## =====================================

# Data:
# - `df_plt_mem` data.frame
load(RES_DIR("fig-cat-memory.Rda"))
# - `df_plt_run` data.frame
load(RES_DIR("fig-cat-runtime.Rda"))


## MEMORY
df_plt_mem = df_cat_memory %>%
  filter(method != "linear") %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mem = median(mem)) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  mutate(method = as.factor(method)) %>%
  filter(ptotal %in% c(10, 40, 75, 100, 150, 300))

levels(df_plt_mem$method) = list(Binary = "binary", Ridge = "ridge")
df_plt_mem$ptotal = ordered(df_plt_mem$ptotal, levels = sort(unique(df_plt_mem$ptotal)))
df_plt_mem$nclasses = factor(paste0("# Classes: ", df_plt_mem$nclasses), levels = paste0("# Classes: ", c(5, 10, 20)))

gg = ggplot() +
  geom_smooth(
    data = df_plt_mem,
    aes(x = nrows, y = mem, color = ptotal, group = paste0(ncols, ncolsnoise, nclasses)),
    se = FALSE) +
  geom_point(
    data = df_plt_mem,
    aes(x = nrows, y = mem, color = ptotal, group = paste0(ncols, ncolsnoise)),
    size = 3,
    alpha = 0.5) +
  scale_x_continuous(breaks = sort(unique(df_plt_mem$nrows)), trans = "log10") +
  xlab("Number of rows\n(log10 Scale)") +
  ylab("Used memory in MB") +
  labs(color = "Number of\nreatures") +
  coord_cartesian(clip = 'off') +
  MY_COLOR + MY_FILL +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(nclasses ~ method)#, scales= "free_y")

if (SAVEGG) {
  ggsave(
    plot = gg,
    filename = FIG_DIR("fig-appendix-cat-memory.pdf"),
    width = 0.75*DINA4WIDTH ,
    height = 0.75*DINA4WIDTH,
    units = "mm")
}

## RUNTIME

df_plt_run = df_cat_runtime %>%
  filter(method != "linear") %>%
  #group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  #summarize(mem = median(time)) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  mutate(method = as.factor(method)) %>%
  group_by(nrows, ptotal, method, nclasses) %>%
  summarize(med = median(time), min = min(time), max = max(time)) %>%
  filter(ptotal %in% c(10, 40, 75, 100, 150, 300))

levels(df_plt_run$method) = list(Binary = "binary", Ridge = "ridge")
df_plt_run$ptotal = ordered(df_plt_run$ptotal, levels = sort(unique(df_plt_run$ptotal)))

df_plt_run$nclasses = factor(paste0("# Classes: ", df_plt_run$nclasses), levels = paste0("# Classes: ", c(5, 10, 20)))

dodge_width = 0.25

gg = ggplot() +
  geom_point(
    data = df_plt_run,
    aes(x = nrows, y = med / 60, color = as.factor(ptotal)),
    size = 1.4,
    alpha = 0.5,
    position = position_dodge(width = dodge_width)) +
  geom_errorbar(
    data = df_plt_run,
    aes(x = nrows, ymax = max / 60, ymin = min / 60, color = as.factor(ptotal)),
    na.rm = TRUE,
    position = position_dodge(width = dodge_width),
    size = 0.4) +
  MY_COLOR + MY_FILL +
  xlab("Number of rows\n(log10 Scale)") +
  ylab("Runtime in minutes") +
  labs(color = "Number of\nfeatures", fill = "Number of\nfeatures") +
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(nclasses ~ method)#, scales= "free_y")

if (SAVEGG) {
  ggsave(
    plot = gg,
    filename = FIG_DIR("fig-appendix-cat-runtime.pdf"),
    width = DINA4WIDTH * 0.75,
    height = DINA4WIDTH * 0.75,
    units = "mm")
}


# Data:
# - `ll_noise` list with
#   $ .. `density` data.frame
#   $ .. `cat_sel` data.frame
load(RES_DIR("fig-cat-noise.Rda"))

gg = ggplot(
    mapping = aes(
      x = rel_nwrongnotselected,
      y = rel_notselected,
      shape = method,
      color = method,
      fill = method)) +
  geom_polygon(
    data = ll_noise$density,
    alpha = 0.2,
    size = 0.1) +
  geom_point(data = ll_noise$cat_sel, size = 0.2) +
  xlab("FNR") +
  ylab("TNR") +
  labs(fill = "", color = "", shape = "") +
  MY_COLOR + MY_FILL +
  xlim(min(ll_noise$density$rel_nwrongnotselected), max(ll_noise$denstiy$rel_nwrongnotselected)) +
  ylim(min(ll_noise$density$rel_notselected), max(ll_noise$density$rel_notselected)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  facet_grid(sn_ratiof ~ .)#, scales = "free_y")

if (SAVEGG) {
  ggsave(
    plot = gg,
    filename = FIG_DIR("fig-appendix-cat-noise.pdf"),
    width = DINA4WIDTH  * 0.7,
    height = DINA4WIDTH  * 0.5,
    units = "mm")
}


# Data:
# - `df_cat_bp` data.frame
load(RES_DIR("fig-cat-mse.Rda"))

font_scale = 6

gg = ggplot(df_cat_bp, aes(x = mse, y = value, fill = method, color = method)) +
  geom_boxplot(alpha = 0.2, size = 0.2, outlier.size = 0.2) +
  xlab("") +
  ylab("MSE") +
  labs(fill = "", color = "", shape = "") +
  MY_COLOR + MY_FILL +
  ylim(0, 40) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_grid(sn_ratiof ~ .) #, scales = "free_y")

if (SAVEGG) {
  ggsave(
    plot = gg,
    filename = FIG_DIR("fig-appendix-cat-mse.pdf"),
    width = DINA4WIDTH *  0.7,
    height = DINA4WIDTH *  0.6,
    units = "mm")
}

}
