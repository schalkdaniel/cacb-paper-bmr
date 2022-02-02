library(dplyr)
library(ggplot2)
library(ggpubr)
library(tidyr)
library(viridis)

source("../../R/bm-sim-data.R")
source("../../R/helper.R")

font = "TeX Gyre Bonum"

sysfonts::font_add(font,
    #regular = paste0(base_dir, "/paper-figures/gyre-bonum/texgyrebonum-regular.ttf"),
    #bold = paste0(base_dir, "/paper-figures/gyre-bonum/texgyrebonum-bold.ttf"))
    regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
    bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
#showtext::showtext_auto()
extrafont::font_import(paths = "~/repos/bm-CompAspCboost/paper-figures/gyre-bonum", prompt = FALSE)
extrafont::loadfonts()

theme_set(
  theme_minimal(base_family = font) +
  ggplot2::theme(
    #strip.background = element_rect(fill = rgb(47, 79, 79, maxColorValue = 255), color = "white"),
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", face = "bold", size = 6),
    axis.text = element_text(size = 9),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 9),
    legend.text = element_text(size = 7),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
)
dinA4width = 162

font_scale = 3

## memory
## ----------------------------------------------

# Load local data:
# -------------------

files = c(list.files("memory", full.name = TRUE), list.files("../binning/memory", full.name = TRUE))
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

mem_setup = 50

for (fn in files) {
  #load(paste0("memory/", fn))
  load(fn)
  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)
  dat_noise = dat$data

  set.seed(bm_extract$data_seed * bm_extract$config$rep)
  dat_noise$y = rnorm(n = bm_extract$config$n, mean = dat_noise$y, sd = sd(dat_noise$y) / bm_extract$config$sn_ratio)

  mem_rsim = sum(c(object.size(dat), object.size(dat_noise))) / 1024^2

  if (grepl("binning", fn)) {
    ll_rows[[k]] = data.frame(
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      #rep         = bm_extract$config$rep,    # rep is always 1 for memory
      ncolsnoise  = bm_extract$config$pnoise,
      mem         = last(bm_extract$ms_extract_nobinning$mem_heap_B) - mem_setup - mem_rsim,
      unit        = last(bm_extract$ms_extract_nobinning$unit),
      method      = "COD"
    )
  } else {
    ll_rows[[k]] = data.frame(
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      #rep         = bm_extract$config$rep,    # rep is always 1 for memory
      ncolsnoise  = bm_extract$config$pnoise,
      mem         = last(bm_extract$ms_extract_agbm$mem_heap_B) - mem_setup - mem_rsim,
      unit        = last(bm_extract$ms_extract_agbm$unit),
      method      = "AGBM"
    )
  }
  k = k+1
}
df_optim_memory = do.call("rbind", ll_rows)


# Plot real memory as lines:
# --------------------------

## NOT RELEVANT!!!

#gg = df_binning_memory %>%
#df_binning_memory %>%
  #ggplot(aes(x = nrows, y = mem / 1024, color = method)) +
    #geom_line() +
    #geom_point() +
    #theme_minimal(base_family = "Gyre Bonum") +
    #theme(
      #strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      #strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      #axis.text.x = element_text(angle = 45, vjust = 0.5),
      #axis.text = element_text(size = 8 * font_scale),
      #axis.title = element_text(size = 10 * font_scale)
    #) +
    #scale_x_continuous(breaks = sort(unique(df_binning_memory$nrows))[-c(1,2)]) +
    #scale_color_viridis(discrete = TRUE) +
    #xlab("Number of Rows") +
    #ylab("Allocated Memory in GB") +
    #labs(color = "") +
    #facet_grid(ncolsnoise ~ ncols, scales = "free_y")

#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "memory_lines.pdf", width = dinA4width * 2/3 * 0.6, height = dinA4width * 2/3, units = "mm")



# Plot used memory (proportional):
# --------------------------------

df_plt_mem = df_optim_memory %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  group_by(nrows, ncolsnoise, ncols) %>%
  summarize(rel_mem = mean_mem[method == "COD"] / mean_mem[method == "AGBM"], ptotal = ncols[1] + ncolsnoise[1])

df_plt_mem$ptotal = factor(df_plt_mem$ptotal, levels = as.character(sort(unique(df_plt_mem$ptotal))))

gg = ggplot(data = df_optim_memory, aes(x = as.factor(nrows), y = mem, color = paste0(method, ncols))) + geom_boxplot()
gg


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
  scale_x_continuous(
    breaks = sort(unique(df_plt_mem$nrows)),
    trans = "log10") +
  scale_y_continuous(breaks = c(1, 2, 4, 6)) +
  scale_color_viridis(discrete = TRUE) +
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
    size = 1.5 * font_scale)




ggplot(data = df_plt_mem %>% filter(rel_mem < 1.1), aes(x = as.factor(nrows), y = rel_mem, color = as.factor(ncols), fill = as.factor(ncols))) +
  geom_boxplot(alpha = 0.2)

gg = ggplot(data = df_plt_mem, aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_hline(yintercept = 1, col = "dark red", lty = 2) +
  geom_line() +
  geom_point() +
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
  scale_x_continuous(breaks = sort(unique(df_optim_memory$nrows))) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows") +
  ylab("Relative improvement of\nallocated memory\nMem(COD) / Mem(AGBM)") +
  labs(color = "Number of\nFeatures") #+
  #facet_grid(. ~ ncols, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "optim_memory_rel_lines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")

tmp = df_optim_memory %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(mem)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal) %>%
  select(nrows, ptotal, COD, AGBM) %>%
  filter(COD == max(COD)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = COD / AGBM)

knitr::kable(round(tmp, 2), format = "latex")


## Complexity estimation of AGBM:
## ----------------------------------------------

files = list.files(here::here("bm-scripts/optimizer/runtime"), full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  load(fn)
  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time_init   = c(bm_extract$time_cod["init.elapsed"], bm_extract$time_agbm["init.elapsed"]),
    time_fit   = c(bm_extract$time_cod["fit.elapsed"], bm_extract$time_agbm["fit.elapsed"]),
    method      = c("COD", "AGBM")
  )
  k = k+1
}
df_optim_runtime = do.call("rbind", ll_rows)

df_comp_acwb = df_optim_runtime %>%
  filter(method == "AGBM") %>%
  mutate(ptotal = ncols + ncolsnoise, time = time_init + time_fit) %>%
  select(nrows, ptotal, time)

save(df_comp_acwb, file = "df_comp_acwb.Rda")

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
1 - sum((df_tmp2$time_est - df_tmp2$time)^2) / sum((df_tmp2$time - mean(df_tmp$time))^2)

#mycolors = c(ggsci::pal_aaas()(3), ggsci::pal_uchicago()(9))

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
  #scale_x_continuous(trans = "log10") +
  ggsci::scale_color_uchicago()
  #scale_color_manual(values = mycolors)

ggsave(gg_comp, filename = "fig-h4.pdf", width = dinA4width * 0.7, height = dinA4width * 0.25, units = "mm")

system("evince fig-h4.pdf &")

## runtime
## ----------------------------------------------

files = list.files("runtime")
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  load(paste0("runtime/", fn))
  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time_init   = c(bm_extract$time_cod["init.elapsed"], bm_extract$time_agbm["init.elapsed"]),
    time_fit   = c(bm_extract$time_cod["fit.elapsed"], bm_extract$time_agbm["fit.elapsed"]),
    method      = c("COD", "AGBM")
  )
  k = k+1
}
df_optim_runtime = do.call("rbind", ll_rows)


df_plt_run = df_optim_runtime %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise) %>%
  summarize(
    rel_time_init = time_init[method == "COD"] / time_init[method == "AGBM"],
    rel_time_fit = time_fit[method == "COD"] / time_fit[method == "AGBM"],
    rel_time = time[method == "COD"] / time[method == "AGBM"],
    ptotal = ncols[1] + ncolsnoise[1]
  ) %>%
  gather(key = "phase", value = "rel_time", starts_with("rel_time"))

df_plt_run$phase[df_plt_run$phase == "rel_time"] = "Initialization + Fitting"
df_plt_run$phase[df_plt_run$phase == "rel_time_init"] = "Initialization"
df_plt_run$phase[df_plt_run$phase == "rel_time_fit"] = "Fitting"
df_plt_run$phase = factor(df_plt_run$phase, levels = c("Initialization + Fitting", "Initialization", "Fitting"))

df_plt_run$ptotal = factor(df_plt_run$ptotal, levels = as.character(sort(unique(df_plt_run$ptotal))))


#gg = ggplot(data = df_plt_run %>% filter(rel_time < 10, rel_time > 1), aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal))) +
gg = ggplot(data = df_plt_run %>% filter(phase == "Fitting"), aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal))) +
  geom_hline(yintercept = 1, lty = 2, col = "dark red") +
  geom_violin(alpha = 0.2) +
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
  xlab("Number of Rows") +
  ylab("Speedup\nTime(COD) / Time(AGBM)") +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures")# +
  #facet_grid(. ~ phase, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "optim_runtime_rel_violines_fitting.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")

tmp = df_optim_runtime %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, ncolsnoise, method) %>%
  summarize(mean_mem = median(time)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal) %>%
  select(nrows, ptotal, COD, AGBM) %>%
  filter(COD == max(COD)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal) %>%
  mutate(rel = COD / AGBM)

tmp$COD = tmp$COD / 60
tmp$AGBM = tmp$AGBM / 60

knitr::kable(round(tmp, 2), format = "latex")





## performance
## ----------------------------------------------


files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
ll_rows_paths = list()
k = 1

summaryFile = function (file) {
  load(file)
  cat("Data: n=", bm_extract$config$n, ", p=", bm_extract$config$p, "\n\n")
  cat("Momentum: ", bm_extract$momentum, "\n\n")
  cat("COD Iterations: ", length(bm_extract$trace_cod), "\n")
  cat("AGBM Iterations: ", length(bm_extract$trace_agbm), "\n\n")
  cat("COD Risk: ", min(bm_extract$log_cod$oob_risk), "\n")
  cat("AGBM Risk: ", min(bm_extract$log_agbm$oob), "\n\n")
}

summaryFile(files[12000])

for (fn in files) {
  cat("Read - ", k, "/", length(files), "\n")

  load(fn)

  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)
  oob_int = mean((mean(dat$data$y) - dat$data$y)^2)

  ll_rows[[k]] = data.frame(
    file        = fn,
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time_init   = c(bm_extract$time_cod["init.elapsed"], bm_extract$time_agbm["init.elapsed"]),
    time_fit   = c(bm_extract$time_cod["fit.elapsed"], bm_extract$time_agbm["fit.elapsed"]),
    method      = c("COD", "AGBM"),
    #iterations  = c(length(bm_extract$log_cod$oob), length(bm_extract$log_agbm$oob)),
    iterations  = c(which.min(bm_extract$log_cod$oob), which.min(bm_extract$log_agbm$oob)),
    min_oob     = c(min(bm_extract$log_cod$oob), min(bm_extract$log_agbm$oob)),
    mom         = bm_extract$momentum,
    #oob_int_min = c(-diff(c(bm_extract$log_cod$oob[1], min(bm_extract$log_cod$oob))), -diff(c(bm_extract$log_agbm$oob[1], min(bm_extract$log_agbm$oob))))
    oob_int_min = c(-diff(c(oob_int, min(bm_extract$log_cod$oob))), -diff(c(oob_int, min(bm_extract$log_agbm$oob))))
  )
  #oob_cod = bm_extract$log_cod$oob
  #oob_agbm = bm_extract$log_agbm$oob

  #iter_cod = as.integer(seq(1, length(oob_cod), length.out = 100L))
  #iter_agbm = as.integer(seq(1, length(oob_agbm), length.out = 100L))

  #ll_rows_paths[[k]] = data.frame(
    #file        = fn,
    #date        = bm_extract$date,
    #data_seed   = bm_extract$data_seed,
    #nrows       = bm_extract$config$n,
    #ncols       = bm_extract$config$p,
    #sn_ratio    = bm_extract$config$sn_ratio,
    #rep         = bm_extract$config$rep,
    #ncolsnoise  = bm_extract$config$pnoise,
    #method      = c(rep("COD", length(iter_cod)), rep("AGBM", length(iter_agbm))),
    #oob         = c(oob_cod[iter_cod], oob_agbm[iter_agbm]),
    #iter        = c(iter_cod, iter_agbm)
  #)
  k = k+1
}

save(ll_rows, file = "ll_rows_agbm_iter.Rda")
load("ll_rows_agbm_iter.Rda")


df_agbm = do.call("rbind", ll_rows)
#df_agbm_paths = do.call("rbind", ll_rows_paths)

load("ll_rows_agbm_mise.Rda")

df_agbm_mise = do.call(rbind, ll_rows) %>%
  mutate(method = optimizer) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, mom, method) %>%
  select(mimse)

mom_bins = 20L

df_plt = df_agbm %>%
  mutate(
    rsq = 1 - min_oob / (oob_int_min + min_oob),
    mom_cat = cut(mom, breaks = seq(min(mom), max(mom), length.out = mom_bins + 1)),
    relnoise = ifelse(ncolsnoise / ncols == 0.4, 0.5, ncolsnoise / ncols)
  ) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, mom) %>%
  left_join(df_agbm_mise) %>%
  summarize(
    diffiter = iterations[method == "COD"] - iterations[method == "AGBM"],
    diffrsq  = rsq[method == "COD"] - rsq[method == "AGBM"],
    reliter  = iterations[method == "COD"] / iterations[method == "AGBM"],
    mom_cat  = mom_cat[1],
    iter_cod = iterations[method == "COD"],
    relnoise = relnoise[1],
    diffmise = mimse[method == "COD"] - mimse[method == "AGBM"],
    file = file[1]
  )

xl = "as.factor(ncols)"

gg_iter = df_plt %>%
  #filter(iter_cod < 50000) %>%
  filter(sn_ratio %in% c(0.1, 1, 10)) %>%
  filter(! ((sn_ratio == 0.1) & (reliter >= 50))) %>%
  na.omit() %>%
  ggplot(aes_string(x = xl, y = "reliter", fill = "mom_cat")) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal(base_family = "Gyre Bonum") +
    #scale_fill_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Number of columns") +
    ylab("Iterations(COD) / Iterations(AGBM)") +
    labs(fill = "Momentum range") +
    facet_grid(paste0("SNR: ", sn_ratio) ~ ., scales = "free_y")

gg_rsq = df_plt %>%
  #filter(iter_cod < 50000) %>%
  filter(sn_ratio %in% c(0.1, 1, 10)) %>%
  filter(diffrsq < 0.1) %>%
  na.omit() %>%
  ggplot(aes_string(x = xl, y = "diffrsq", fill = "mom_cat")) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal(base_family = "Gyre Bonum") +
    #scale_fill_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Number of columns") +
    ylab(expression(R^2~(COD) - R^2~(AGBM))) +
    labs(fill = "Momentum range") +
    facet_grid(paste0("SNR: ", sn_ratio) ~ ., scales = "free_y")

gg_mise = df_plt %>%
  #filter(iter_cod < 50000) %>%
  filter(sn_ratio %in% c(0.1, 1, 10)) %>%
  na.omit() %>%
  filter(!is.nan(diffmise)) %>%
  ggplot(aes_string(x = xl, y = "diffmise", fill = "mom_cat")) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal(base_family = "Gyre Bonum") +
    #scale_fill_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Number of columns") +
    ylab("MISE(COD) - MISE(AGBM)") +
    labs(fill = "Momentum range") +
    facet_grid(paste0("SNR: ", sn_ratio) ~ ., scales = "free_y")

gridExtra::grid.arrange(gg_iter, gg_rsq, gg_mise, ncol = 3L)

dinA4width = 210 * font_scale
ggsave(plot = gg_iter, filename = "agbm_iterations.pdf", width = dinA4width, height = 2/3 * dinA4width, units = "mm")
ggsave(plot = gg_rsq, filename = "agbm_rsq.pdf", width = dinA4width, height = 2/3 * dinA4width, units = "mm")
ggsave(plot = gg_mise, filename = "agbm_mise.pdf", width = dinA4width, height = 2/3 * dinA4width, units = "mm")


coef_names = paste0("coef_", c("cod", "agbm"))
feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)






plotFeatures = function (fn, diffmise) {
  load(fn)
  coef_names = paste0("coef_", c("cod", "agbm"))
  feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

  ll_fe = lapply(feat_effects, function (df) {
    df %>%
      pivot_longer(cols = c(coef_names, "truth"), names_to = "optimizer", values_to = "effect") %>%
      group_by(optimizer) %>%
      mutate(y = effect - mean(effect)) %>%
      arrange(optimizer, x)
  })
  df_fe = do.call(rbind, ll_fe)

  feat_id = as.integer(gsub("\\D", "", df_fe$bl))
  feat = paste0("Feature ", feat_id)
  df_fe$feat = factor(feat, levels = paste0("Feature ", sort(unique(feat_id))))

  df_fe$line = df_fe$optimizer
  df_fe$line[df_fe$line == "truth"] = "Truth"
  df_fe$line[df_fe$line == "coef_cod"] = "COD"
  df_fe$line[df_fe$line == "coef_agbm"] = "AGBM"

  ggplot(df_fe, aes(x = x, y = y, color = line)) +
    geom_line() +
    #scale_color_viridis(discrete = TRUE) +
    theme_minimal(base_family = "Gyre Bonum") +
    scale_color_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      axis.text.x=element_blank(),
      axis.ticks.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks.y=element_blank()
    ) +
    xlab("Feature Value") +
    ylab("Estimated Feature Effect") +
    labs(color = "") +
    facet_wrap(. ~ feat, scales = "free") +
    ggtitle(paste0("Rows: ", bm_extract$config$n, " SNR: ", bm_extract$config$sn_ratio, " Iterations (COD/AGBM): ", length(bm_extract$trace_cod), "/", length(bm_extract$trace_agbm), " Noise: ", bm_extract$config$pnoise, " Momentum: ", bm_extract$mom, " Diffmise: ", diffmise))
}


df_worse = df_plt %>%
  ungroup() %>%
  filter(!is.nan(diffmise)) %>%
  filter(sn_ratio == 10) %>%
  filter(between(diffmise, -20, 17)) %>%
  filter(diffmise == min(diffmise)) %>%
  as.data.frame()

plot(density(df_worse$diffmise))
dim(df_worse)
files = df_worse$file



dinA4width = 210 * font_scale
k = 1
for(fn in files) {
  gg  = plotFeatures(fn, df_worse$diffmise[df_worse$file == fn])
  ggsave(plot = gg, filename = paste0("fe/fe", k ,".pdf"), width = dinA4width, height = 2/3 * dinA4width, units = "mm")
  k = k + 1
}


# Base-learner traces:

plotTraces = function (fn){
  load(fn)

  gg_cod = plotBlearnerTraces(bm_extract$trace_cod, n_legend = bm_extract$config$p) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    ggtitle(paste0("Rows: ", bm_extract$config$n, " Cols: ", bm_extract$config$p, " SNR: ", bm_extract$config$sn_ratio, " Noise: ", bm_extract$config$pnoise, " Momentum: ", bm_extract$mom, " Diffmise: ", df_plt$diffmise[df_plt$file == fn]))

  gg_agbm1 = plotBlearnerTraces(bm_extract$trace_agbm, n_legend = bm_extract$config$p) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("AGBM trace") +
    theme(legend.position = "none")

  gg_agbm2 = plotBlearnerTraces(bm_extract$trace_agbm_mom, n_legend = bm_extract$config$p) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    ggtitle("Momentum trace") +
    theme(legend.position = "none")

  gridExtra::grid.arrange(gg_cod, gridExtra::grid.arrange(gg_agbm1, gg_agbm2, ncol = 2), ncol = 1)
}

plotTraces(df_plt$file[12000])
plotFeatures(df_plt$file[12000], df_plt$diffmise[12000])
plotTraces(df_plt$file[3000])
plotTraces(df_plt$file[6600])
plotTraces(df_plt$file[9870])





a = df_plt %>%
  filter(diffrsq > 0, diffrsq < 0.05)

#a$iter_cat = cut(x = a$reliter, breaks = seq(0, max(a$reliter), length.out = 11))
#a$iter_cat = cut(x = a$reliter, breaks = quantile(a$reliter, probs = seq(0, 1, length.out = 5)))
a$iter_cat = cut(x = a$reliter, breaks = c(1, 2, 5, 10, 20, 50, 100, Inf))
a$mise_cat = cut(x = a$diffmise, breaks = quantile(a$diffmise, probs = seq(0, 1, length.out = 11)))

na.omit(a) %>%
  filter(sn_ratio %in% c(0.1, 1, 10)) %>%
  ggplot(aes(x = as.factor(ncols), y = diffrsq,  fill = iter_cat)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal(base_family = "Gyre Bonum") +
    #scale_fill_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Number of columns") +
    ylab(expression(R^2~(COD) - R^2~(AGBM))) +
    labs(fill = "Iteration speedup range\nIter(COD) / Iter(AGBM)") +
    facet_grid(paste0("SNR: ", sn_ratio) ~ ., scales = "free_y")

na.omit(a) %>%
  filter(sn_ratio %in% c(0.1, 1, 10)) %>%
  ggplot(aes(x = as.factor(ncols), y = diffmise,  fill = iter_cat)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal(base_family = "Gyre Bonum") +
    #scale_fill_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Number of columns") +
    ylab(expression(R^2~(COD) - R^2~(AGBM))) +
    labs(fill = "Iteration speedup range\nIter(COD) / Iter(AGBM)") +
    facet_grid(paste0("SNR: ", sn_ratio) ~ ., scales = "free_y")

na.omit(a) %>%
  filter(sn_ratio %in% c(0.1, 1, 10)) %>%
  ggplot(aes(x = as.factor(ncols), y = diffrsq,  fill = mise_cat)) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    theme_minimal(base_family = "Gyre Bonum") +
    #scale_fill_brewer(palette = "Set1") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Number of columns") +
    ylab(expression(R^2~(COD) - R^2~(AGBM))) +
    labs(fill = "MISE differnece range\nMISE(COD) - MISE(AGBM)") +
    facet_grid(paste0("SNR: ", sn_ratio) ~ ., scales = "free_y")













df_plt = df_agbm %>%
  mutate(time = time_init + time_fit) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, mom) %>%
  summarize(
    #rel_time_init = time_init[method == "COD"] / time_init[method == "AGBM"],
    #rel_time_fit = time_fit[method == "COD"] / time_fit[method == "AGBM"],
    rel_time = time[method == "COD"] / time[method == "AGBM"],
    ptotal = ncols[1] + ncolsnoise[1],
    diffiter = (iterations[method == "COD"] - iterations[method == "AGBM"]) / iterations[method == "COD"],
    diffoob  = (min_oob[method == "COD"] - min_oob[method == "AGBM"]) / min_oob[method == "COD"],
    diffiter_t = (iterations[method == "COD"] - iterations[method == "AGBM"]),
    diffoob_t  = (min_oob[method == "COD"] - min_oob[method == "AGBM"]),
    diffoob_int = (oob_int_min[method == "COD"] - oob_int_min[method == "AGBM"]) / oob_int_min[method == "COD"],
    diffoob_int_t = oob_int_min[method == "COD"] - oob_int_min[method == "AGBM"],
    range_cod = oob_int_min[method == "COD"],
    iter_cod = iterations[method == "COD"],
    range_agbm = oob_int_min[method == "AGBM"]
  )

#mod_iter = lm(diffiter ~ mom + nrows + ncols + ncolsnoise + as.factor(sn_ratio), data = df_plt)
#summary(mod_iter)
#mod_oob = lm(diffoob ~ mom + nrows + ncols + ncolsnoise  + as.factor(sn_ratio), data = df_plt)
#summary(mod_oob)


#ggplot(df_plt, aes(x = diffiter_t, y = diffoob_int, color = as.factor(ncols))) +
  #geom_point(alpha = 1, size = 2) +
  #facet_grid(nrows ~ sn_ratio, scales = "free_y") +
  #scale_color_viridis(discrete = TRUE)


gg = ggplot() +
  geom_point(data = df_plt %>% filter(diffiter > -1, iter_cod < 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))),
    aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3, stroke = 1) +
  geom_segment(data = df_plt %>% filter(diffiter > -1, iter_cod < 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))),
    aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  #geom_point(data = df_plt, aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3, stroke = 0) +
  #geom_segment(data = df_plt, aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  geom_point(data = df_plt %>% filter(diffiter > -1, iter_cod == 20000, !((sn_ratio == 1) & (diffiter < 0.6)), !((sn_ratio == 10) & (diffiter < 0.75))), aes(x = diffiter, y = range_agbm, color = as.factor(ncols)), alpha = 0.3, stroke = 1, shape = 4, show.legend = FALSE) +
  facet_grid(nrows ~ sn_ratio, scales = "free") +
  #scale_color_viridis(discrete = TRUE)
  scale_color_brewer(palette = "Set1") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale)
  ) +
  xlab("Relative Improvement of Iterations") +
  ylab("Risk Improvement from\nIntercept Model") +
  labs(color = "Number of informative\nfeatures")


#dinA4width = 210 * font_scale
#ggsave(plot = gg, filename = "optim_oob.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")



## Try to find a model for momentum:

df_model = df_agbm %>%
  filter(method == "AGBM") %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  select(nrows, ptotal, mom, sn_ratio, ncols, ncolsnoise)

mod_mom = lm(mom ~ nrows + ncols + ncolsnoise + as.factor(sn_ratio), data = df_model)
summary(mod_mom)

ggplot(data = df_model, aes(x = nrows, y = mom, color = as.factor(ncols))) + geom_point() + geom_smooth(method = "lm", se = FALSE) +
  facet_grid(. ~ ncolsnoise)


ggplot() +
  geom_point(data = df_plt %>% filter(diffiter > 0, iter_cod < 20000), aes(x = diffiter, y = range_cod, color = mom), alpha = 0.3, stroke = 1) +
  geom_segment(data = df_plt %>% filter(diffiter > 0, iter_cod < 20000), aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = mom), alpha = 1) +
  #geom_point(data = df_plt, aes(x = diffiter, y = range_cod, color = as.factor(ncols)), alpha = 0.3, stroke = 0) +
  #geom_segment(data = df_plt, aes(x = diffiter, y = range_cod, xend = diffiter, yend = range_cod - diffoob_int_t, color = as.factor(ncols)), alpha = 1) +
  geom_point(data = df_plt %>% filter(diffiter > 0, iter_cod == 20000), aes(x = diffiter, y = range_agbm, color = mom), alpha = 0.3, stroke = 1, shape = 4, show.legend = FALSE) +
  facet_grid(nrows ~ sn_ratio, scales = "free") +
  #scale_color_viridis(discrete = TRUE)
  #scale_color_brewer(palette = "Set1") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale)
  ) +
  xlab("Relative Improvement of Iterations") +
  ylab("Risk Improvement from\nIntercept Model") +
  labs(color = "Number of informative\nfeatures")






# Performance: Visualize one setting:
files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

fn = files[700]
load(fn)

coef_names = paste0("coef_", c("cod", "agbm"))
feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

ll_fe = lapply(feat_effects, function (df) {
  df %>%
    pivot_longer(cols = c(coef_names, "truth"), names_to = "optimizer", values_to = "effect") %>%
    group_by(optimizer) %>%
    mutate(y = effect - mean(effect)) %>%
    arrange(optimizer, x)
})
df_fe = do.call(rbind, ll_fe)

feat_id = as.integer(gsub("\\D", "", df_fe$bl))
feat = paste0("Feature ", feat_id)
df_fe$feat = factor(feat, levels = paste0("Feature ", sort(unique(feat_id))))

df_fe$line = df_fe$optimizer
df_fe$line[df_fe$line == "truth"] = "Truth"
df_fe$line[df_fe$line == "coef_cod"] = "COD"
df_fe$line[df_fe$line == "coef_agbm"] = "AGBM"


ggplot(df_fe, aes(x = x, y = y, color = line)) +
  geom_line() +
  #scale_color_viridis(discrete = TRUE) +
  theme_minimal(base_family = "Gyre Bonum") +
  scale_color_brewer(palette = "Set1") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale)
  ) +
  xlab("Feature Value") +
  ylab("Estimated Feature Effect") +
  labs(color = "") +
  facet_wrap(. ~ feat, scales = "free") +
  ggtitle(paste0("Rows: ", bm_extract$config$n, " SNR: ", bm_extract$config$sn_ratio))





coef_names = paste0("coef_", c("cod", "agbm"))
feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

ll_imse = lapply(feat_effects, function (df) {
  df %>%
    pivot_longer(cols = coef_names, names_to = "optimizer", values_to = "effect") %>%
    group_by(optimizer) %>%
    mutate(y = effect - mean(effect), truth = truth - mean(truth)) %>%
    arrange(optimizer, x)
})
df_imse = do.call(rbind, ll_imse)

df_imse_agg = df_imse %>%
  group_by(bl, optimizer) %>%
  summarize(
    mse = mean((truth - y)^2),
    imse = getFeatureIME(x = x, truth = truth, pred = y),
    mae = mean(abs(truth - y)),
    imae = getFeatureIME(x = x, truth = truth, pred = y, loss = function (f,y) abs(f - y))
  ) %>%
  group_by(optimizer) %>%
  summarize(
    mmse = mean(mse),
    mimse = mean(imse),
    mmae = mean(mae),
    mimae = mean(imae)
  )

df_imse_agg

files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  cat("Read - ", k, "/", length(files), "\n")

  load(fn)

  df_temp = data.frame(
    file        = fn,
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    iterations  = c(which.min(bm_extract$log_cod$oob), which.min(bm_extract$log_agbm$oob)),
    min_oob     = c(min(bm_extract$log_cod$oob), min(bm_extract$log_agbm$oob)),
    mom         = bm_extract$momentum,
    optimizer   = c("COD", "AGBM")
  )

  coef_names = paste0("coef_", c("cod", "agbm"))
  feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

  ll_imse = lapply(feat_effects, function (df) {
    df %>%
      pivot_longer(cols = coef_names, names_to = "optimizer", values_to = "effect") %>%
      group_by(optimizer) %>%
      mutate(y = effect - mean(effect), truth = truth - mean(truth)) %>%
      arrange(optimizer, x)
  })
  df_imse = do.call(rbind, ll_imse)

  df_imse_agg = df_imse %>%
    group_by(bl, optimizer) %>%
    summarize(
      mse = mean((truth - y)^2),
      imse = getFeatureIME(x = x, truth = truth, pred = y),
      mae = mean(abs(truth - y)),
      imae = getFeatureIME(x = x, truth = truth, pred = y, loss = function (f,y) abs(f - y))
    ) %>%
    group_by(optimizer) %>%
    summarize(
      mmse = mean(mse, na.rm = TRUE),
      mimse = mean(imse, na.rm = TRUE),
      mmae = mean(mae, na.rm = TRUE),
      mimae = mean(imae, na.rm = TRUE)
    ) %>%
    mutate(optimizer = ifelse(optimizer == "coef_cod", "COD", "AGBM"))

  ll_rows[[k]] = df_temp %>%
    left_join(df_imse_agg, by = "optimizer")

  k = k + 1
}
save(ll_rows, file = "ll_rows_agbm_mise.Rda")
load("ll_rows_agbm_mise.Rda")


df_imse = do.call(rbind, ll_rows)
df_imse

df_imse$ptotal = df_imse$ncols + df_imse$ncolsnoise
ggplot(df_imse, aes(x = as.factor(sn_ratio), y = mimse, color = optimizer)) +
  geom_violin() +
  facet_grid(nrows ~ ncols + ncolsnoise)





# Load local files:
# -----------------

files = list.files("performance")
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

#for (fn in files) {
  #load(paste0("performance/", fn))
  #e = try(expr = {
    #bl_mses = getBLMSE(bm_extract)
    #data.frame(
      #file        = fn,
      #date        = bm_extract$date,
      #data_seed   = bm_extract$data_seed,
      #nrows       = bm_extract$config$n,
      #ncols       = bm_extract$config$p,
      #sn_ratio    = bm_extract$config$sn_ratio,
      #rep         = bm_extract$config$rep,
      #ncolsnoise  = bm_extract$config$pnoise,
      #time_init   = c(bm_extract$time_nobinning["init.elapsed"], bm_extract$time_binning["init.elapsed"]),
      #time_fit   = c(bm_extract$time_nobinning["fit.elapsed"], bm_extract$time_binning["fit.elapsed"]),
      #method      = c("nobinning", "binning"),
      #bl_mse      = unlist(bl_mses)
    #)}, silent = TRUE
  #)

  #if (class(e) == "try-error") cat(paste0(k, " - ", fn, ": ", e)) else ll_rows[[k]] = e
  #k = k+1
#}
load("ll_rows_performance.Rda")
df_binning_performance = do.call("rbind", ll_rows)

# Boxplot of base-learner mses:
# -----------------------------


p = ggboxplot(df_binning_performance, x = "nrows", y = "bl_mse",
  color = "method", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  add = "jitter", shape = "method")

my_comparisons = list( c("nobinning", "binning"))
p = p + stat_compare_means(comparisons = my_comparisons) + # Add pairwise comparisons p-value
  stat_compare_means(label.y = 50)                   # Add global p-value

facet(p, facet.by = c("ncolsnoise", "ncols"),
      short.panel.labs = FALSE)



# Visualize one base-learner of a specific setting:
# -------------------------------------------------

# Here the setting 100:
fn = files[100]
load(paste0("performance/", fn))

# choose base-learner for visualization:
bl_tab = table(bm_extract$trace_binning)
bl = "x9_spline"

coefs_binning = bm_extract$coef_binning[[bl]]
coefs_nobinning = bm_extract$coef_nobinning[[bl]]

mean((coefs_binning - coefs_nobinning)^2)

# Plot estimated effect vs truth:
df_plot = getFeatEffectData(bm_extract,bl)
ggplot(data = df_plot, aes(x = x, y = y, color = method)) + geom_line()

# Oob risk of binning vs nobinning:
df_risk = getOobRiskData(bm_extract)
ggplot(data = df_risk, aes(x = iter, y = risk, color = method, linetype = method)) + geom_line()


# Plot estimated effect over the 20 replications:
fn_pre = "xxx-n50000-p50-pnoise50-snr1-"
files_fix = grep(fn_pre, files)
bl = "x7_spline"

load(paste0("performance/", files[files_fix[1]]))
df_base = getFeatEffectData(bm_extract, bl, TRUE)

set.seed(bm_extract$data_seed * bm_extract$config$rep)
df_base$ynoise = rnorm(n = bm_extract$config$n, mean = df_base$y, sd = sd(df_base$y) / bm_extract$config$sn_ratio)

ll_effect = list()
k = 1
for (fn in files_fix) {
  load(paste0("performance/", files[fn]))
  ll_effect[[k]] = getFeatEffectData(bm_extract, bl, FALSE)
  ll_effect[[k]]$rep = k
  k = k + 1
}

gg = ggplot() +
  geom_line(data = df_base %>% filter(method == "truth"), aes(x = x, y = y), color = "dark red")

for (efct in ll_effect) {
  gg = gg + geom_line(data = efct, aes(x = x, y = y, color = method), alpha = 0.2)
}
gg + scale_color_viridis(discrete = TRUE)







files = list.files("agbm-mom", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  cat("Read - ", k, "/", length(files), "\n")

  load(fn)

  set.seed(bm_extract$data_seed)
  dat = simData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise)
  dat_noise = dat$data

  risk_int = mean((dat_noise$y - mean(dat_noise$y))^2)

    df_temp = data.frame(
      file        = fn,
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      rep         = bm_extract$config$rep,
      ncolsnoise  = bm_extract$config$pnoise,
      mom         = bm_extract$momentum,
      time_init   = c(bm_extract$time_cod["init.elapsed"], bm_extract$time_agbm["init.elapsed"]),
      time_fit   = c(bm_extract$time_cod["fit.elapsed"], bm_extract$time_agbm["fit.elapsed"]),
      iterations  = c(which.min(bm_extract$log_cod$oob), which.min(bm_extract$log_agbm$oob)),
      min_oob     = c(min(bm_extract$log_cod$oob), min(bm_extract$log_agbm$oob)),
      risk_int    = rep(risk_int, 2),
      method      = c("COD", "AGBM")
    )

    coef_names = paste0("coef_", c("cod", "agbm"))
    feat_effects = getAllFeatEffectData(bm_extract, ndata = bm_extract$config$n / 4,  coef_names = coef_names)

    ll_imse = lapply(feat_effects, function (df) {
      df %>%
        pivot_longer(cols = all_of(coef_names), names_to = "method", values_to = "effect") %>%
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
      mutate(method = ifelse(method == "coef_agbm", "AGBM", "COD"))

    ll_rows[[k]] = df_temp %>%
      left_join(df_imse_agg, by = "method")

    k = k + 1
}
save(ll_rows, file = "ll_rows_optimizer_measures.Rda")
load("ll_rows_optimizer_measures.Rda")
df_imse = do.call(rbind, ll_rows)

gg_optim_mise = df_imse %>%
  group_by(nrows, ncols, ncolsnoise, sn_ratio, rep, mom) %>%
  summarize(mimse_diff = mimse[method == "COD"] - mimse[method == "AGBM"]) %>%
  group_by(nrows, ncols, ncolsnoise, sn_ratio, mom) %>%
  summarize(
    mimse_diffm = median(mimse_diff),
    mm_upper = median(mimse_diff) + sd(mimse_diff),
    mm_lower = median(mimse_diff) - sd(mimse_diff)
  ) %>%
  ggplot(aes(x = nrows, y = mimse_diffm, color = mom)) +#, shape = as.factor(ncols))) +
    #geom_boxplot(alpha = 0.5) +
    geom_hline(yintercept = 0, color = "dark red", linetype = "dashed") +
    geom_jitter(width = 0.04, alpha = 0.4, size = 3) +
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
  scale_color_viridis() +
  scale_fill_viridis(discrete = TRUE) +
  scale_x_continuous(breaks = c(5000, 10000,20000,50000,100000), trans = "log10") +
  xlab("Number of Rows") +
  ylab("MISE(COD) - MISE(AGBM)") + #"\n(MISE = Mean Integrated Squared Error)") +
  labs(color = "Momentum", fill = "", shape = "Number of\nInformative\nFeatures") +
  facet_grid(paste0("SNR = ", sn_ratio) ~ factor(paste0("# Features: ",ncols), levels = paste0("# Features: ", c(5, 10, 20, 50))), scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg_optim_mise, filename = "optim_mise_fe.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")




fn = files[500]
load(fn)

coef_names = paste0("coef_", c("cod", "agbm"))
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
df_fe$line[df_fe$line == "coef_cod"] = "COD"
df_fe$line[df_fe$line == "coef_agbm"] = "AGBM"


gg = ggplot(df_fe, aes(x = x, y = y, color = line)) +
  geom_line() +
  #scale_color_viridis(discrete = TRUE) +
  theme_minimal(base_family = "Gyre Bonum") +
  scale_color_brewer(palette = "Set1") +
  theme(
    strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white")  ,
    strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
    axis.text.x = element_text(angle = 45, vjust = 0.5),
    axis.text = element_text(size = 8 * font_scale),
    axis.title = element_text(size = 10 * font_scale),
    legend.text = element_text(size = 6 * font_scale),
    legend.title = element_text(size = 8 * font_scale)
  ) +
  xlab("Feature Value") +
  ylab("Estimated Feature Effect") +
  labs(color = "") +
  scale_x_continuous(breaks = NULL) +
  facet_wrap(. ~ feat, scales = "free") +
  ggtitle(paste0("Rows: ", bm_extract$config$n, " SNR: ", bm_extract$config$sn_ratio))

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "optim_single_fe.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.7, units = "mm")







df_plt = df_imse %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, mom) %>%
  summarize(
    ptotal = ncols[1] + ncolsnoise[1],
    diffiter = (iterations[method == "COD"] - iterations[method == "AGBM"]) / iterations[method == "COD"],
    diffoob  = (min_oob[method == "COD"] - min_oob[method == "AGBM"]) / min_oob[method == "COD"],
    diffiter_t = (iterations[method == "COD"] - iterations[method == "AGBM"]),
    diffoob_t  = (min_oob[method == "COD"] - min_oob[method == "AGBM"]),
    iter_cod = iterations[method == "COD"],
    risk_explained = ((risk_int[1] - min_oob[method == "COD"]) - (risk_int[1] - min_oob[method == "AGBM"])) / risk_int[1]
  )

gg_oob = df_plt %>%
  mutate(ncolsf = factor(paste0("# Features: ", ncols), levels = paste0("# Features: ", c(5,10,20,50)))) %>%
  group_by(nrows, ncolsf, sn_ratio, ncolsnoise, mom) %>%
  #filter(iters_bin < 20000, iters_ridge < 20000) %>%
  summarize(risk_explained = mean(risk_explained), diffiter = mean(diffiter_t), ptotal = ptotal[1])%>%
  ggplot(aes(x = risk_explained, y = diffiter, color = mom, shape = factor(nrows))) +
    geom_point(alpha = 0.5, size = 3) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis() +
    #geom_smooth(method = "lm", se = FALSE)+
    #scale_color_brewer(palette = "Set1") +
    xlab("Difference in Explained Risk") +
    ylab("Iterations(COD) - Iterations(AGBM)") +
    labs(color = "Momentum", fill = "Signal-to-Noise Ratio", shape = "Number of Rows") +
    theme_minimal(base_family = "Gyre Bonum") +
    #theme_minimal() +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 10 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)#,
      #legend.position = c(.95, .95),
      #legend.justification = c("right", "top")
    ) +
    facet_grid(paste0("SNR: ", sn_ratio) ~ ncolsf, scales = "free_y")
gg_oob

dinA4width = 210 * font_scale
ggsave(plot = gg_oob, filename = "optim_oob_points.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")




