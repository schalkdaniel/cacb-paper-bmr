library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(viridis)

source("../../R/bm-sim-data.R")
source("../../R/helper.R")

sysfonts::font_add("Gyre Bonum",
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
showtext::showtext_auto()

font_scale = 3

## memory
## ----------------------------------------------

# Load data:
# -------------------

files = list.files("memory", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

mem_setup = 50

for (fn in files) {
  load(fn)

  cat("Read", k, "/", length(files), "\n")

  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)
  dat_noise = dat$data

  set.seed(bm_extract$data_seed * bm_extract$config$rep)
  dat_noise$y = rnorm(n = bm_extract$config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / bm_extract$config$sn_ratio)

  mem_rsim = sum(c(object.size(dat), object.size(dat_noise))) / 1024^2

  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    #rep         = bm_extract$config$rep,    # rep is always 1 for memory
    ncolsnoise  = bm_extract$config$pnoise,
    mem         = c(last(bm_extract$ms_extract_nobinning$mem_heap_B), last(bm_extract$ms_extract_binning$mem_heap_B)) - mem_setup - mem_rsim,
    unit        = c(last(bm_extract$ms_extract_nobinning$unit), last(bm_extract$ms_extract_binning$unit)),
    method      = c("no binning", "binning")
  )
  k = k+1
}
df_binning_memory = do.call("rbind", ll_rows)


# Plot used memory (proportional):
# --------------------------------

df_plt_mem = df_binning_memory %>%
  #filter(ncolsnoise == 10) %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  group_by(nrows, ncolsnoise, ncols) %>%
  summarize(rel_mem = mean_mem[method == "no binning"] / mean_mem[method == "binning"], ptotal = ncols[1] + ncolsnoise[1])

df_plt_mem$ptotal = factor(df_plt_mem$ptotal, levels = as.character(sort(unique(df_plt_mem$ptotal))))

font_scale = 6

#gg = ggplot(data = df_plt_mem, aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
ggplot(data = df_plt_mem, aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_hline(yintercept = 1, col = "dark red", lty = 2) +
  geom_line() +
  geom_point(size = 4) +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale)
  ) +
  scale_x_continuous(breaks = sort(unique(df_binning_memory$nrows)), trans = "log10") +
  scale_y_continuous(breaks = c(1, 2, 4, 6)) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows\n(log10 Scale)") +
  ylab("Relative improvement of\nallocated memory\nMem(No Binning) / Mem(Binning)") +
  labs(color = "Number of\nFeatures") +
  annotate("text", x = max(df_plt_mem$nrows), y = 1, label = "Baseline (used memory is equal)", color = "dark red", vjust = 1.5, hjust = 1)

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "binning_memory_rel_lines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.7, units = "mm")

font_scale = 3

tmp = df_binning_memory %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise, nobinning = `no binning`) %>%
  group_by(nrows, ptotal) %>%
  select(nrows, ptotal, nobinning, binning) %>%
  filter(nobinning == max(nobinning)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = nobinning / binning)

knitr::kable(round(tmp, 2), format = "latex")


## runtime
## ----------------------------------------------

files = list.files("runtime", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  load(fn)
  cat("Read", k, "/", length(files), "\n")
  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time_init   = c(bm_extract$time_nobinning["init.elapsed"], bm_extract$time_binning["init.elapsed"]),
    time_fit   = c(bm_extract$time_nobinning["fit.elapsed"], bm_extract$time_binning["fit.elapsed"]),
    method      = c("nobinning", "binning")
  )
  k = k+1
}
df_binning_runtime = do.call("rbind", ll_rows)



ll_res = list()
ll_est = list()
ll_res_bin = list()
ll_est_bin = list()
for (nrow0 in unique(df_binning_runtime$nrows)) {
  df_cwb0 = df_binning_runtime %>%
    filter(method == "nobinning", nrows == nrow0) %>%
    mutate(time = time_fit + time_init, ptotal = ncols + ncolsnoise)
  df_cwb = df_cwb0 %>%
    group_by(ptotal, nrows, method) %>%
    summarize(time = median(time))

  df_bin0 = df_binning_runtime %>%
    filter(method == "binning", nrows == nrow0) %>%
    mutate(time = time_fit + time_init, ptotal = ncols + ncolsnoise)
  df_bin = df_bin0 %>%
    group_by(ptotal, nrows, method) %>%
    summarize(time = median(time))

  # CWB: $\mathcal{O}(J(\bd^2n + \bd^3) + MJ(\bd^2 + dn))$
  ffun = function (pars, dat) {
    return(with(dat, (ptotal^pars["a0"] * (20^pars["a"]*nrows^pars["c0"] + 20^pars["b"]) + 200^pars["b0"] * ptotal^pars["a1"] *(20^pars["d"] + 20^pars["e"] * nrows^pars["c1"])) / pars["v"]))
  }
  # Binning: $\mathcal{O}(J(\bd^2n^\ast  + n + \bd^3) + MJ(\bd^2 + \bd n^\ast + n))$
  ffun_bin = function (pars, dat) {
    return(with(dat, (ptotal^pars["a0"] * (20^pars["a"]*sqrt(nrows)^pars["b0"] + nrows^pars["b1"] + 20^pars["b"]) + 200^pars["c0"] * ptotal^pars["a1"] * (20^pars["d"] + 20^pars["e"] * sqrt(nrows)^pars["b2"] + nrows^pars["b3"])) / pars["v"]))
  }
  fopt = function (pars, dat, target, fun) {
    y = fun(pars, dat)
    y0 = dat[[target]]
    return (mean(abs(y - y0)))
  }
  diffs = ffun(c(a = 2, b = 3, d = 2, e = 1, v = 1, a0 = 1, a1 = 1, b0 = 1, c0 = 1, c1 = 1), df_cwb) / df_cwb[["time"]]
  diffs_bin = ffun_bin(c(a = 2, b = 3, d = 2, e = 1, v = 1, a0 = 1, a1 = 1, b0 = 1, b1 = 1, b2 = 1, b3 = 1, c0 = 1), df_bin) / df_bin[["time"]]

  par_start = c(a = 2, b = 3, d = 2, e = 1, v = mean(diffs), a0 = 1, a1 = 1, b0 = 1, c0 = 1, c1 = 1)
  par_start_bin = c(a = 2, b = 3, d = 2, e = 1, v = mean(diffs_bin), a0 = 1, a1 = 1, b0 = 1, b1 = 1, b2 = 1, b3 = 1, c0 = 1)

  est = optim(par = par_start, fn = fopt, dat = df_cwb, target = "time", fun = ffun,  method = "Nelder-Mead")
  est_bin = optim(par = par_start_bin, fn = fopt, dat = df_bin, target = "time", fun = ffun_bin,  method = "Nelder-Mead")

  fit = ffun(est$par, df_cwb0)
  fit_bin = ffun_bin(est_bin$par, df_bin0)

  ll_res[[as.character(nrow0)]] = data.frame(d = df_cwb0$ptotal, y = c(df_cwb0$time, fit),
    what = rep(c("Time", "Estimate"), each = length(fit)), nrow = nrow0)
  ll_est[[as.character(nrow0)]] = est
  ll_res_bin[[as.character(nrow0)]] = data.frame(d = df_bin0$ptotal, y = c(df_bin0$time, fit_bin),
    what = rep(c("Time", "Estimate"), each = length(fit_bin)), nrow = nrow0)
  ll_est_bin[[as.character(nrow0)]] = est_bin
}

pars_cwb = rowMeans(sapply(ll_est, function (x) x$par))
pars_bin = rowMeans(sapply(ll_est_bin, function (x) x$par))


df_plt = do.call(rbind, ll_res)
df_plt0 = df_plt  %>%
  filter(what == "Time") %>%
  group_by(d, nrow) %>%
  summarize(med = median(y), upper = quantile(y, 0.99), lower = quantile(y, 0.01))

gg_cwb = ggplot() +
  geom_point(data = df_plt, aes(x = d, y = y, color = as.factor(nrow)), alpha = 0.2, stroke = 0.5, size = 8, shape = 4) +
  #geom_point(data = df_plt0, aes(x = d, y = med, color = as.factor(nrow))) +
  #geom_linerange(data = df_plt0, aes(x = d, ymin = lower, ymax = upper, color = as.factor(nrow))) +
  geom_line(data = df_plt %>% filter(what == "Estimate"), aes(x = d, y = y, color = as.factor(nrow)), size = 2) +
  scale_x_continuous(trans = "log10") +
  labs(color = "n") +
  xlab("d") +
  ylab("Time in seconds") +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale),
    panel.grid.major.x = element_blank(),
    plot.subtitle = element_text(size=10 * font_scale),
    plot.title = element_text(size=12 * font_scale),
    legend.position = "bottom") +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE)

df_plt_bin = do.call(rbind, ll_res_bin)
df_plt_bin0 = df_plt_bin  %>%
  filter(what == "Time") %>%
  group_by(d, nrow) %>%
  summarize(med = median(y), upper = quantile(y, 0.99), lower = quantile(y, 0.01))

gg_bin = ggplot() +
  geom_point(data = df_plt_bin, aes(x = d, y = y, color = as.factor(nrow)), alpha = 0.2, stroke = 0.5, size = 8, shape = 4) +
  #geom_point(data = df_plt_bin0, aes(x = d, y = med, color = as.factor(nrow))) +
  #geom_linerange(data = df_plt_bin0, aes(x = d, ymin = lower, ymax = upper, color = as.factor(nrow))) +
  geom_line(data = df_plt_bin %>% filter(what == "Estimate"), aes(x = d, y = y, color = as.factor(nrow)), size = 2) +
  scale_x_continuous(trans = "log10") +
  labs(color = "n") +
  xlab("d") +
  ylab("Time in seconds") +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size=12 * font_scale),
    plot.subtitle = element_text(size=10 * font_scale),
    legend.position = "bottom") +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE)


#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend = function(a.gplot){
  tmp = ggplot_gtable(ggplot_build(a.gplot))
  leg = which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend = tmp$grobs[[leg]]
  return(legend)}

mylegend = g_legend(gg_cwb)

library(gridExtra)
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
  heights = c(10, 1))






df_plt_run = df_binning_runtime %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise) %>%
  summarize(
    rel_time_init = time_init[method == "nobinning"] / time_init[method == "binning"],
    rel_time_fit = time_fit[method == "nobinning"] / time_fit[method == "binning"],
    rel_time = time[method == "nobinning"] / time[method == "binning"],
    ptotal = ncols[1] + ncolsnoise[1]
  ) %>%
  gather(key = "phase", value = "rel_time", starts_with("rel_time"))

df_plt_run$phase[df_plt_run$phase == "rel_time"] = "Initialization + Fitting"
df_plt_run$phase[df_plt_run$phase == "rel_time_init"] = "Initialization"
df_plt_run$phase[df_plt_run$phase == "rel_time_fit"] = "Fitting"
df_plt_run$phase = factor(df_plt_run$phase, levels = c("Initialization + Fitting", "Initialization", "Fitting"))

df_plt_run$ptotal = factor(df_plt_run$ptotal, levels = as.character(sort(unique(df_plt_run$ptotal))))


font_scale = 3

gg = ggplot() +
#ggplot() +
  geom_hline(yintercept = 1, lty = 2, col = "dark red") +
  geom_violin(data = df_plt_run %>% filter(rel_time < 10, rel_time > 2, phase == "Fitting"), aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal)), alpha = 0.2, show.legend = FALSE) +
  theme_minimal(base_family = "Gyre Bonum") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_viridis(discrete=TRUE) +
  scale_fill_viridis(discrete=TRUE) +
  xlab("Number of Rows\n(log10 Scale)") +

  ylab(expression(atop("Speedup\n", paste(Time["No Binning"], "/", Time["Binning"], sep = "")))) +
  #ylab("Speedup\nTime(No Binning) / Time(Binning)") +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures") +
  scale_y_continuous(breaks = c(1, 2, 4, 6)) +
  geom_text(data = data.frame(x = 6, y = 1, label = "Baseline (runtime is equal)", phase = "Initialization + Fitting", ptotal = 250),
    aes(x = x, y = y, label = label), color = "dark red", vjust = 1.5, hjust = 1, show.legend = FALSE, size = 4)
  #facet_grid(. ~ factor(phase, levels = c("Initialization", "Fitting", "Initialization + Fitting")), scales = "free_y")

dinA4width = 210 * font_scale / 2
scale_fac = 2/3
#scale_fac = 0.5
ggsave(plot = gg, filename = "binning_runtime_rel_violines.pdf", width = dinA4width * scale_fac, height = dinA4width * 0.8 * scale_fac, units = "mm")

font_scale = 3

tmp = df_binning_runtime %>%
  mutate(time = time_init + time_fit) %>%
  filter(rep == 1) %>%
  select(nrows, ncols, ncolsnoise, sn_ratio, time, method) %>%
  pivot_wider(names_from = method, values_from = time) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  select(nrows, ptotal, nobinning, binning) %>%
  group_by(nrows, ptotal) %>%
  filter(nobinning == max(nobinning)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = nobinning / binning)

tmp$nobinning = tmp$nobinning / 60
tmp$binning = tmp$binning / 60
knitr::kable(round(tmp, 2), format = "latex")


## performance
## ----------------------------------------------

seed = 31415

n = 10000
p = 4
pnoise = 2
sn_ratio = 0.4

set.seed(seed)
dat = simData(n, p, pnoise)
dat_noise = dat$data
set.seed(seed)
dat_noise$y = rnorm(n = n, mean = dat_noise$y, sd = sd(dat_noise$y) / sn_ratio)

library(compboost)

set.seed(seed)
cboost = boostSplines(data = dat_noise, target = "y", iterations = 10000L, learning_rate = 0.01,
  loss = LossQuadratic$new(), stop_args = list(eps_for_break = 0, patience = 3L), oob_fraction = 0.3,
  df = 7)

set.seed(seed)
cboost_bin = boostSplines(data = dat_noise, target = "y", iterations = 10000L, learning_rate = 0.01,
  loss = LossQuadratic$new(), stop_args = list(eps_for_break = 0, patience = 3L), oob_fraction = 0.3,
  bin_root = 2, df = 7)



ndata = 1000L
dat_idx = as.integer(seq(1, n, len = ndata))

feat = colnames(dat$data)[grepl(pattern = "x", x = colnames(dat$data))]
bls = paste0(feat, "_spline")
coef_names = c("coef_binning", "coef_nobinning")
coefs = list(coef_binning = cboost_bin$getEstimatedCoef(), coef_nobinning = cboost$getEstimatedCoef())

out = list()
for(bl in bls) {
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

ll_fe = lapply(out, function (df) {
  df %>%
    pivot_longer(cols = all_of(c(coef_names, "truth")), names_to = "method", values_to = "effect") %>%
    group_by(method) %>%
    mutate(y = effect - mean(effect)) %>%
    arrange(method, x)
})
df_fe = do.call(rbind, ll_fe)

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

gg = ggplot() +
  geom_ribbon(
    data = df_area,
    aes(ymin = y, ymax = y_t, x = x),
    fill = "dark red",
    alpha = 0.4) +
  geom_line(
    data = df_fe,
    aes(x = x, y = y, color = line, linetype = linetype),
    lwd = 2) +
  #scale_color_viridis(discrete = TRUE) +
  theme_minimal(base_family = "Gyre Bonum") +
  scale_color_viridis(discrete = TRUE) +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 8 * font_scale)
      #axis.text.x=element_blank(),
      #axis.ticks.x=element_blank(),
      #axis.text.y=element_blank(),
      #axis.ticks.y=element_blank()
    ) +
  xlab("Feature Value") +
  ylab("Estimated Feature Effect") +
  labs(color = "", linetype = "") +
  #scale_x_continuous(breaks = NULL) +
  scale_linetype(guide = "none") +
  facet_wrap(. ~ feat, scales = "free_x")#, scales = "free")
gg




# Performance: Visualize one setting:
files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

#fn = "performance/xxx-n10000-p10-pnoise20-snr0.1-rep3-df5-binroot2.Rda"
fn = "performance/xxx-n50000-p10-pnoise20-snr0.1-rep3-df9-binroot2.Rda"

#fn = files[100]
load(fn)

coef_names = paste0("coef_", c("binning", "nobinning"))
feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

ll_fe = lapply(feat_effects, function (df) {
  df %>%
    pivot_longer(cols = all_of(c(coef_names, "truth")), names_to = "method", values_to = "effect") %>%
    group_by(method) %>%
    mutate(y = effect - mean(effect)) %>%
    arrange(method, x)
})
df_fe = do.call(rbind, ll_fe)

feat_id = as.integer(gsub("\\D", "", df_fe$bl))
feat = paste0("Feature ", feat_id)
df_fe$feat = factor(feat, levels = paste0("Feature ", sort(unique(feat_id))))

df_fe$line = df_fe$method
df_fe$line[df_fe$line == "truth"] = "Truth"
df_fe$line[df_fe$line == "coef_binning"] = "Binning"
df_fe$line[df_fe$line == "coef_nobinning"] = "No Binning"


gg = ggplot(df_fe, aes(x = x, y = y, color = line)) +
  geom_line(lwd = 2) +
  #scale_color_viridis(discrete = TRUE) +
  theme_minimal(base_family = "Gyre Bonum") +
  scale_color_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 8 * font_scale),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    ) +
  xlab("Feature Value") +
  ylab("Estimated Feature Effect") +
  labs(color = "") +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(. ~ feat, scales = "free") +
  ggtitle(paste0("Rows: ", bm_extract$config$n, " SNR: ", bm_extract$config$sn_ratio))
gg
dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "binning_performance_effects1.pdf", width = dinA4width, height = dinA4width, units = "mm")




## Calculate mean integrated squared error and visualize:
## ----------------------------------------------------------

files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  cat("Read - ", k, "/", length(files), "\n")

  load(fn)

  if (! is.null(bm_extract$trace_binning)) {
    df_temp = data.frame(
      file        = fn,
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      rep         = bm_extract$config$rep,
      ncolsnoise  = bm_extract$config$pnoise,
      df          = bm_extract$df,
      bin_root    = bm_extract$bin_root,
      iterations  = c(which.min(bm_extract$log_binning$oob), which.min(bm_extract$log_nobinning$oob_risk)),
      min_oob     = c(min(bm_extract$log_binning$oob), min(bm_extract$log_nobinning$oob_risk)),
      method = c("Binning", "No Binning")
    )

    coef_names = paste0("coef_", c("binning", "nobinning"))
    feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

    ll_imse = lapply(feat_effects, function (df) {
      df %>%
        pivot_longer(cols = coef_names, names_to = "method", values_to = "effect") %>%
        group_by(method) %>%
        mutate(y = effect - mean(effect), truth = truth - mean(truth)) %>%
        arrange(method, x)
    })
    df_imse = do.call(rbind, ll_imse)

    df_imse_agg = df_imse %>%
      group_by(bl, method) %>%
      summarize(
        mse = mean((truth - y)^2),
        imse = getFeatureIME(x = x, truth = truth, pred = y),
        mae = mean(abs(truth - y)),
        imae = getFeatureIME(x = x, truth = truth, pred = y, loss = function (f,y) abs(f - y))
      ) %>%
      group_by(method) %>%
      summarize(
        mmse = mean(mse, na.rm = TRUE),
        mimse = mean(imse, na.rm = TRUE),
        mmae = mean(mae, na.rm = TRUE),
        mimae = mean(imae, na.rm = TRUE)
      ) %>%
      mutate(method = ifelse(method == "coef_binning", "Binning", "No Binning"))

    ll_rows[[k]] = df_temp %>%
      left_join(df_imse_agg, by = "method")

    k = k + 1
  }
}
load("ll_rows_performance_measures.Rda")
df_imse = do.call(rbind, ll_rows)
df_imse

df_imse = df_imse %>%
  mutate(method_n = ifelse(method == "Binning", paste0(method, " ", bin_root), method)) %>%
  filter(bin_root != 9) %

  ggplot(df_imse, aes(x = as.factor(nrows), y = mimse, fill = method_n, color = method_n)) +
    geom_boxplot(alpha = 0.5) +
    theme_minimal(base_family = "Gyre Bonum") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale),
    panel.grid.major.x = element_blank()
  ) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE) +
  xlab("Number of Rows") +
  ylab("Mean Integrated Squared Error") +
  labs(color = "", fill = "") +
  facet_grid(paste0("SNR = ", sn_ratio) ~ paste0("df = ", df), scales = "free_y")


dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "binning_performance_boxplots.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")









