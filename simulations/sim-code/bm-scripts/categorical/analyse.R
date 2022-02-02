library(dplyr)
library(ggplot2)
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

# Load local data:
# -------------------

files = list.files("memory", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()

mem_setup = 50

k = 1
for (fn in files) {
  load(fn)

  cat("Read", k, "/", length(files), "\n")

  set.seed(bm_extract$data_seed)
  dat = simCategoricalData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise, nclasses = bm_extract$cls_config$ncls[1], ncnoise = bm_extract$cls_config$nic[1])

  cnames = colnames(dat$data)
  for (fn in cnames[cnames != "y"]) {
    dat$data[[fn]] = as.character(dat$data[[fn]])
  }
  dat_noise = dat$data

  mem_rsim = sum(c(object.size(dat), object.size(dat_noise))) / 1024^2

  if(is.null(bm_extract$ms_extract_ridge)) {
    cat(fn, " does not have ridge memory heap size\n")
  } else {
    ll_rows[[k]] = data.frame(
      date        = bm_extract$date,
      data_seed   = bm_extract$data_seed,
      nrows       = bm_extract$config$n,
      ncols       = bm_extract$config$p,
      sn_ratio    = bm_extract$config$sn_ratio,
      nclasses    = bm_extract$cls_config["ncls"][1,1],
      nnoninfocls = bm_extract$cls_config["nic"][1,1],
      #rep         = bm_extract$config$rep,    # rep is always 1 for memory
      ncolsnoise  = bm_extract$config$pnoise,
      mem         = c(last(bm_extract$ms_extract_linear$mem_heap_B), last(bm_extract$ms_extract_binary$mem_heap_B), last(bm_extract$ms_extract_ridge$mem_heap_B)) - mem_setup - mem_rsim,
      unit        = c(last(bm_extract$ms_extract_linear$unit), last(bm_extract$ms_extract_binary$unit), last(bm_extract$ms_extract_ridge$unit)),
      method      = c("linear", "binary", "ridge")
    )
  }
  k = k+1
}
df_cat_memory = do.call("rbind", ll_rows)

# Plot used memory (proportional):
# --------------------------------

df_plt_mem = df_cat_memory %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mem = median(mem)) %>%
  group_by(nrows, ncols, ncolsnoise, nclasses) %>%
  summarize(rel_mem_bin = mem[method == "linear"] / mem[method == "binary"], rel_mem_ridge = mem[method == "linear"] / mem[method == "ridge"], ptotal = ncols[1] + ncolsnoise[1]) %>%
  gather(key = "method", value = "rel_mem", starts_with("rel_mem")) %>%
  filter(rel_mem >= 1)

df_plt_mem$ptotal = factor(df_plt_mem$ptotal, levels = as.character(sort(unique(df_plt_mem$ptotal))))
df_plt_mem$method[df_plt_mem$method == "rel_mem_bin"] = "Binary"
df_plt_mem$method[df_plt_mem$method == "rel_mem_ridge"] = "Ridge"
df_plt_mem$ncls_cat = factor(paste0(df_plt_mem$nclasses, " classes"), levels = paste0(sort(unique(df_plt_mem$nclasses)), " classes"))

gg = ggplot() +
  geom_hline(yintercept = 1, col = "dark red", lty = 2) +
  geom_line(data = df_plt_mem, aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_point(data = df_plt_mem, aes(x = nrows, y = rel_mem, color = ptotal, group = paste0(ncols, ncolsnoise))) +
  geom_text(data = data.frame(x = 100000, y = 1, label = "Baseline (used memory is equal)", method = "Ridge", ptotal = 250, ncls_cat = factor("20 classes", levels = paste0(c(5, 10, 20), " classes"))),
    aes(x = x, y = y, label = label), color = "dark red", vjust = 1.5, hjust = 1, show.legend = FALSE) +
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
  scale_x_continuous(breaks = sort(unique(df_cat_memory$nrows)), trans = "log10") +
  scale_color_viridis(discrete = TRUE) +
  xlab("Number of Rows\n(log10 Scale)") +
  ylab("Relative Deviation of the\nAllocated Memory in MB\nCompared to Using No Binning") +
  labs(color = "Number of\nFeatures") +
  coord_cartesian(clip = 'off') +
  facet_grid(ncls_cat ~ method, scales= "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "categorical_memory_rel_lines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")


tmp = df_cat_memory %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mean_mem = median(mem)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method, nclasses) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal, nclasses) %>%
  select(nrows, ptotal, linear, binary, ridge, nclasses) %>%
  filter(linear == max(linear), nclasses %in% c(5, 20)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal, nclasses) #%>%
  #mutate(rel = nobinning / binning)

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

  ncls = as.integer(strsplit(x = strsplit(x = fn, split = "nclasses")[[1]][2], split = "-informative")[[1]][1])
  nic = as.integer(strsplit(x = strsplit(x = fn, split = "informative-classes")[[1]][2], split = ".Rda")[[1]][1])
  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    nclasses    = ncls,
    nnoninfocls = bm_extract$config_classes["nic"][1,1],
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    time        = c(sum(bm_extract$time_linear), sum(bm_extract$time_binary), sum(bm_extract$time_ridge)),
    method      = c("linear", "binary", "ridge")
  )
  k = k+1
}
df_cat_runtime = do.call("rbind", ll_rows)

df_plt_run = df_cat_runtime %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, nclasses) %>%
  summarize(
    rel_time_binary = time[method == "linear"] / time[method == "binary"],
    rel_time_ridge = time[method == "linear"] / time[method == "ridge"],
    ptotal = ncols[1] + ncolsnoise[1]
  ) %>%
  gather(key = "method", value = "rel_time", starts_with("rel_time"))

df_plt_run$ptotal = factor(df_plt_run$ptotal, levels = as.character(sort(unique(df_plt_run$ptotal))))
df_plt_run$method[df_plt_run$method == "rel_time_binary"] = "Binary"
df_plt_run$method[df_plt_run$method == "rel_time_ridge"] = "Ridge"
df_plt_run$ncls_cat = factor(paste0(df_plt_run$nclasses, " classes"), levels = paste0(sort(unique(df_plt_run$nclasses)), " classes"))

gg = ggplot() +
  geom_hline(yintercept = 1, lty = 2, col = "dark red") +
  geom_violin(data = df_plt_run, aes(x = as.factor(nrows), y = rel_time, fill = as.factor(ptotal), color = as.factor(ptotal)), alpha = 0.5) +
  geom_text(data = data.frame(x = 6, y = 1, label = "Baseline (runtime is equal)", method = "Ridge", ptotal = 250, ncls_cat = factor("20 classes", levels = paste0(c(5, 10, 20), " classes"))),
    aes(x = x, y = y, label = label), color = "dark red", vjust = 1.5, hjust = 1, show.legend = FALSE) +
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
  ylab("Relative Deviation of the\nRuntime in Seconds\nCompared to Using No Binning") +
  labs(color = "Number of\nFeatures", fill = "Number of\nFeatures") +
  coord_cartesian(clip = 'off') +
  facet_grid(ncls_cat ~ method, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "categorical_runtime_rel_violines.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.5, units = "mm")



tmp = df_cat_runtime %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses) %>%
  summarize(mean_mem = median(time)) %>%
  select(nrows, ncols, ncolsnoise, mean_mem, method, nclasses) %>%
  pivot_wider(names_from = method, values_from = mean_mem) %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  group_by(nrows, ptotal, nclasses) %>%
  select(nrows, ptotal, linear, binary, ridge, nclasses) %>%
  filter(linear == max(linear), nclasses %in% c(5, 20)) %>%
  group_by(nrows) %>%
  filter(ptotal %in% c(min(ptotal), max(ptotal))) %>%
  arrange(nrows, ptotal, nclasses) #%>%
  #mutate(rel = nobinning / binning)

tmp$binary = tmp$binary / 60
tmp$linear = tmp$linear / 60
tmp$ridge  = tmp$ridge / 60
knitr::kable(round(tmp, 2), format = "latex")


## -----------------------------------------------------------------
## performance
## -----------------------------------------------------------------

files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files[20001:length(files)]) {
  cat(as.character(Sys.time()), "Read: ", k , "/", length(files), "\n")

  load(fn)

  selected_feat_bin = unlist(lapply(strsplit(x = bm_extract$trace_binary, split = "_"), function (x) x[1]))
  selected_feat_ridge = unlist(lapply(strsplit(x = bm_extract$trace_ridge, split = "_"), function (x) x[1]))
  ncls = as.integer(strsplit(x = strsplit(x = fn, split = "nclasses")[[1]][2], split = "-informative")[[1]][1])

  n_noise_bin = sum(grepl(x = selected_feat_bin, pattern = "noise"))
  n_feat_bin = length(selected_feat_bin) - n_noise_bin

  n_noise_ridge = sum(grepl(x = selected_feat_ridge, pattern = "noise"))
  n_feat_ridge = length(selected_feat_ridge) - n_noise_ridge

  set.seed(bm_extract$data_seed)
  dat = simCategoricalData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise, nclasses = bm_extract$config_classes$ncls[1], ncnoise = bm_extract$config_classes$nic[1])
  oob_int = mean((mean(dat$data$y) - dat$data$y)^2)

  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    nclasses    = ncls,
    nnoninfocls = bm_extract$config_classes["nic"][1,1],
    time_init   = c(bm_extract$time_binary["init.elapsed"], bm_extract$time_ridge["init.elapsed"]),
    time_fit   = c(bm_extract$time_binary["fit.elapsed"], bm_extract$time_ridge["fit.elapsed"]),
    method      = c("binary", "ridge"),
    iterations = c(length(selected_feat_bin), length(selected_feat_ridge)),
    oob_risk_int = oob_int,
    oob_risk_min = c(min(bm_extract$log_binary$logger_data[,2]), min(bm_extract$log_ridge$logger_data[,2])),
    n_selected   = c(n_feat_bin, n_feat_ridge),
    n_noise      = c(n_noise_bin, n_noise_ridge)
  )
  k = k+1
}
save(ll_rows, file = "ll_rows_cat.Rda")
load("ll_rows_cat.Rda")

df_cat = do.call("rbind", ll_rows)



# Selected noise in fitting process:
# ------------------------------------------

gg_sel = df_cat %>%
  pivot_longer(names_to = "feat_type", values_to = "selected", cols = starts_with("n_")) %>%
  mutate(
    rel_selected = selected / iterations,
    ft = ifelse(feat_type == "n_selected", "Informative", "Noise")
  ) %>%
  #group_by(nrows, ncols, ncolsnoise, ft, method, sn_ratio, nclasses) %>%
  group_by(nrows, ncols, ncolsnoise, ft, method, sn_ratio) %>%
  filter(ft == "Informative") %>%
  #summarize(rel = median(rel_selected), min_rel = min(rel_selected), max_rel = max(rel_selected)) %>%
  summarize(
    rel = median(rel_selected),
    min_rel = median(rel_selected) - sd(rel_selected),
    max_rel = median(rel_selected) + sd(rel_selected),
    pn_rel = ncolsnoise[1] / ncols[1]
  ) %>%
  mutate(pn_rel = ifelse(pn_rel == 0.4, 0.5, pn_rel)) %>%
  #filter(ncols == 50, nclasses == 20) %>%
    ggplot(aes(x = nrows, y = rel, linetype = method, color = as.factor(sn_ratio))) +
      geom_linerange(aes(ymin = min_rel, ymax = max_rel), alpha = 0.5, position = position_dodge(width = 0.05)) +
      geom_line(position = position_dodge(width = 0.05)) +
      #scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Set1") +
      xlab("Number of Rows\n(log10 Scale)") +
      ylab("Relative Amount of Selected\nInformative Feature\nDuring the Fitting Process") +
      labs(linetype = "Method", fill = "Method", color = "Signal to Noise\nRatio") +
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
      scale_x_continuous(breaks = unique(df_cat$nrows), trans = "log10") +
      facet_grid(pn_rel  ~ ncols)#, scales = "free_y")
gg_sel

dinA4width = 210 * font_scale
ggsave(plot = gg_sel, filename = "categorical_selection_full.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")



# Trace how base-learner are selected:
# ------------------------------------------

load("performance/xxx-n100000-p10-pnoise20-snr1-rep1-nclasses10-informative-classes0.Rda")
bl_ridge= vapply(bm_extract$trace_ridge, FUN.VALUE = character(1L), FUN = function (b) strsplit(x = b, split = "_")[[1]][1])
gg1 = plotBlearnerTraces(bl_ridge, n_legend = 10L) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)

bl_binary = vapply(bm_extract$trace_binary, FUN.VALUE = character(1L), FUN = function (b) strsplit(x = b, split = "_")[[1]][1])
gg2 = plotBlearnerTraces(bl_binary, n_legend = 10L) +
  scale_color_viridis(discrete = TRUE) +
  scale_fill_viridis(discrete = TRUE)

gridExtra::grid.arrange(gg1, gg2)



# Comparison of explained risk:
# -----------------------------------------

df_plt = df_cat  %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, nclasses, nnoninfocls) %>%
  summarize(
    ptotal = ncols[1] + ncolsnoise[1],
    diffiter = (iterations[method == "binary"] - iterations[method == "ridge"]) / iterations[method == "binary"],
    diffoob  = (oob_risk_min[method == "binary"] - oob_risk_min[method == "ridge"]) / oob_risk_min[method == "binary"],
    diffiter_t = (iterations[method == "binary"] - iterations[method == "ridge"]),
    diffoob_t  = (oob_risk_min[method == "binary"] - oob_risk_min[method == "ridge"]),
    diffoob_int = (oob_risk_int[method == "binary"] - oob_risk_int[method == "ridge"]) / oob_risk_int[method == "binary"],
    diffoob_int_t = oob_risk_int[method == "binary"] - oob_risk_int[method == "ridge"],
    range_cod = oob_risk_int[method == "binary"],
    iter_cod = iterations[method == "binary"],
    range_agbm = oob_risk_int[method == "ridge"],
    risk_explained = ((oob_risk_int[method == "ridge"] - oob_risk_min[method == "ridge"]) - (oob_risk_int[method == "binary"] - oob_risk_min[method == "binary"])) / oob_risk_int[method == "ridge"],
    iters_bin = iterations[method == "binary"],
    iters_ridge = iterations[method == "ridge"]
  )

gg_oob = df_plt %>%
  mutate(ncolsf = factor(paste0("# Features: ", ncols), levels = paste0("# Features: ", c(5,10,20,50)))) %>%
  group_by(nrows, ncolsf, sn_ratio, ncolsnoise, nclasses, nnoninfocls) %>%
  #filter(iters_bin < 20000, iters_ridge < 20000) %>%
  summarize(risk_explained = mean(risk_explained), diffiter = mean(diffiter_t), ptotal = ptotal[1]) %>%
  ggplot(aes(x = risk_explained, y = diffiter, color = factor(sn_ratio), shape = factor(nrows))) +
    geom_point(alpha = 0.5, size = 3) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    #geom_smooth(method = "lm", se = FALSE)+
    #scale_color_brewer(palette = "Set1") +
    xlab("Difference in Explained Risk") +
    ylab("Iterations(Binary) - Iterations(Ridge)") +
    labs(color = "Signal-to-Noise Ratio", fill = "Signal-to-Noise Ratio", shape = "Number of Rows") +
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
    facet_grid(factor(paste0("# Classes: ", nclasses), levels = paste0("# Classes: ", c(5,10,20))) ~ ncolsf)

#gg_oob = ggExtra::ggMarginal(gg_oob, type = "density", groupFill = TRUE, groupColour = TRUE, margins = "y")

dinA4width = 210 * font_scale
ggsave(plot = gg_oob, filename = "categorical_oob.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")






# Dependency between Iters(Ridge) ~ Iters(Binary):
# -----------------------------------------------------------

df_cat_iter = df_cat %>%
  select(nrows, ncols, sn_ratio, rep, ncolsnoise, nclasses, nnoninfocls, method, iterations) %>%
  pivot_wider(names_from = "method", values_from = "iterations") %>%
  mutate(ptotal = ncols + ncolsnoise) %>%
  filter(binary < 20000)

ll_temp = list()
k = 1
for (p in unique(df_cat_iter$ptotal)) {
  for (n in unique(df_cat_iter$nrows)) {
    temp = df_cat_iter %>% filter(ptotal == p, nrows == n) %>% mutate(nclasses = as.factor(nclasses)) %>% select(binary, ridge, nclasses, ptotal, nrows)
    mod = lm(binary ~  0 + ridge*nclasses, data = temp)
    params = coef(mod)

    temp_max = temp %>% group_by(nclasses) %>% filter(ridge == max(ridge))

    ll_empty = list()
    for (i in seq_len(nrow(temp_max))) {
      pred = predict(mod, temp_max[i,])
      if (temp_max[i,"nclasses",drop = TRUE] %in% mod$xlevels$nclasses) {
        ie = paste0("ridge:nclasses", temp_max[i,"nclasses",drop=TRUE])
        if (ie %in% names(params)) {
          ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"] + params[ie])
          #if (is.na(coef(summary(mod))[,"Pr(>|t|)"][ie])) {
            #ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"])
          #} else {
            #if (coef(summary(mod))[,"Pr(>|t|)"][ie] < 0.05) {
              #ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"] + params[ie])
            #} else {
              #ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = NA)
            #}
          #}
        } else {
          ll_empty[[i]] = cbind(temp_max[i,], pred = pred, label = params["ridge"])
        }
      }
    }
    preds = do.call(rbind, ll_empty)
    preds$labels = as.character(round(preds$label, 2))
    ll_temp[[k]] = preds
    k = k + 1
  }
}
df_labels = do.call(rbind, ll_temp)

gg_iter = df_cat_iter %>%
  ggplot(aes(x = ridge, y = binary, color = as.factor(nclasses))) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(intercept = 0, slope = 1, color = "dark red", linetype = "dashed", alpha = 0.5) +
    ggrepel::geom_label_repel(data = df_labels, aes(x = ridge, y = binary, fill = factor(nclasses, levels = c("5", "10", "20")), label = labels),
      colour = "white", fontface = "bold", show.legend = FALSE) +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    #scale_color_brewer(palette = "Set1") +
    #scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_family = "Gyre Bonum") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 9 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Iterations (Ridge)") +
    ylab("Iterations (Binary)") +
    labs(color = "Number of\nclasses\nper feature") +
    scale_x_continuous(breaks = c(0, 8000)) +
    facet_grid(factor(paste0("# Rows:\n", nrows), levels = paste0("# Rows:\n", c(5000,10000,20000,50000,100000))) ~ factor(paste0("# p:\n", ptotal), levels = paste0("# p:\n", sort(unique(df_labels$ptotal)))))

dinA4width = 210 * font_scale
ggsave(plot = gg_iter, filename = "categorical_iters.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")

gg_iter_meta = df_labels %>%
  ggplot(aes(x = label, color = nclasses, fill = nclasses)) +
    geom_density(alpha = 0.5) +
    scale_color_viridis(discrete = TRUE) +
    scale_fill_viridis(discrete = TRUE) +
    #scale_color_brewer(palette = "Set1") +
    #scale_fill_brewer(palette = "Set1") +
    theme_minimal(base_family = "Gyre Bonum") +
    theme(
      strip.background = element_rect(fill = rgb(47,79,79,maxColorValue = 255), color = "white"),
      strip.text = element_text(color = "white", face = "bold", size = 8 * font_scale),
      axis.text.x = element_text(angle = 45, vjust = 0.5),
      axis.text = element_text(size = 8 * font_scale),
      axis.title = element_text(size = 9 * font_scale),
      legend.text = element_text(size = 6 * font_scale),
      legend.title = element_text(size = 8 * font_scale)
    ) +
    xlab("Slope: Iterations(Binary) ~ Iterations(Ridge)") +
    ylab("Density") +
    labs(color = "Number of\nclasses\nper feature", fill = "Number of\nclasses\nper feature") +
    facet_wrap(. ~ factor(paste0("# Features: ", ptotal), levels = paste0("# Features: ", sort(unique(df_labels$ptotal)))))

dinA4width = 210 * font_scale
ggsave(plot = gg_iter_meta, filename = "categorical_iters_meta.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")





# Check how well noise categories are "NOT" estimated
# ---------------------------------------------------

#load("performance/xxx-n100000-p5-pnoise5-snr10-rep15-nclasses20-informative-classes10.Rda")

#set.seed(bm_extract$data_seed)
#dat = simCategoricalData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise, nclasses = bm_extract$config_classes$ncls[1], ncnoise = bm_extract$config_classes$nic[1])


#real_params = dat$cat_param

#est_params = bm_extract$coef_ridge[[2]]
#names(est_params) = vapply(names(est_params), FUN.VALUE = character(1L), FUN = function (nm) strsplit(nm, split = "_")[[1]][1])
#est_params = transformRidgeToParam(est_params, dat$data)
#getNoiseMSE(real_params, est_params)

#est_params = bm_extract$coef_binary[[2]]
#est_params = transformBinaryToParam(est_params)
#getNoiseMSE(real_params, est_params)




files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

#for (fn in sample(files, 200, FALSE)) {
for (fn in files) {
  cat(as.character(Sys.time()), "Read: ", k , "/", length(files), "\n")

  if(! grepl(pattern = "informative-classes0", x = fn)) {

  load(fn)

  set.seed(bm_extract$data_seed)
  dat = simCategoricalData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise, nclasses = bm_extract$config_classes$ncls[1], ncnoise = bm_extract$config_classes$nic[1])

  real_params = dat$cat_param

  est_params = bm_extract$coef_ridge[[2]]
  names(est_params) = vapply(names(est_params), FUN.VALUE = character(1L), FUN = function (nm) strsplit(nm, split = "_")[[1]][1])
  est_params = transformRidgeToParam(est_params, dat$data)

  mse_ridge = getNoiseMSE(real_params, est_params, FALSE)
  mse_ridge_wn = getNoiseMSE(real_params, est_params, TRUE)
  mse_ridge_just_wn = getNoiseMSE(real_params, est_params, TRUE, TRUE)

  ridge_cutoff = 0.01
  est_params_ridge_cutoff001 = lapply(est_params, function (p) {
    p$means = ifelse (abs(p$means) > ridge_cutoff, p$means, 0)
    p
  })
  mse_ridge_cutoff001 = getNoiseMSE(real_params, est_params_ridge_cutoff001, FALSE)
  mse_ridge_wn_cutoff001 = getNoiseMSE(real_params, est_params_ridge_cutoff001, TRUE)
  mse_ridge_just_wn_cutoff001 = getNoiseMSE(real_params, est_params_ridge_cutoff001, TRUE, TRUE)

  ridge_cutoff = 0.5
  est_params_ridge_cutoff05 = lapply(est_params, function (p) {
    p$means = ifelse (abs(p$means) > ridge_cutoff, p$means, 0)
    p
  })
  mse_ridge_cutoff05 = getNoiseMSE(real_params, est_params_ridge_cutoff05, FALSE)
  mse_ridge_wn_cutoff05 = getNoiseMSE(real_params, est_params_ridge_cutoff05, TRUE)
  mse_ridge_just_wn_cutoff05 = getNoiseMSE(real_params, est_params_ridge_cutoff05, TRUE, TRUE)

  ridge_cutoff = 1
  est_params_ridge_cutoff1 = lapply(est_params, function (p) {
    p$means = ifelse (abs(p$means) > ridge_cutoff, p$means, 0)
    p
  })
  mse_ridge_cutoff1 = getNoiseMSE(real_params, est_params_ridge_cutoff1, FALSE)
  mse_ridge_wn_cutoff1 = getNoiseMSE(real_params, est_params_ridge_cutoff1, TRUE)
  mse_ridge_just_wn_cutoff1 = getNoiseMSE(real_params, est_params_ridge_cutoff1, TRUE, TRUE)

  est_params = bm_extract$coef_binary[[2]]
  est_params = transformBinaryToParam(est_params)
  mse_binary = getNoiseMSE(real_params, est_params, FALSE)
  mse_binary_wn = getNoiseMSE(real_params, est_params, TRUE)
  mse_binary_just_wn = getNoiseMSE(real_params, est_params, TRUE, TRUE)

  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    nclasses    = bm_extract$config_classes$ncls[1],
    nnoninfocls = bm_extract$config_classes["nic"][1,1],
    time_init   = c(bm_extract$time_binary["init.elapsed"], rep(bm_extract$time_ridge["init.elapsed"], 4)),
    time_fit   = c(bm_extract$time_binary["fit.elapsed"], rep(bm_extract$time_ridge["fit.elapsed"], 4)),
    method      = c("binary", "ridge", "ridge_cutoff001", "ridge_cutoff05", "ridge_cutoff1"),
    mse = c(mse_binary_wn$mean, mse_ridge_wn$mean, mse_ridge_wn_cutoff001$mean, mse_ridge_wn_cutoff05$mean, mse_ridge_wn_cutoff1$mean),
    mse_with_noise  = c(mse_binary$mean, mse_ridge$mean, mse_ridge_cutoff001$mean, mse_ridge_cutoff05$mean, mse_ridge_cutoff1$mean),
    mse_noise = c(mse_binary_just_wn$mean, mse_ridge_just_wn$mean, mse_ridge_just_wn_cutoff001$mean, mse_ridge_just_wn_cutoff05$mean, mse_ridge_just_wn_cutoff1$mean),
    nnotselected = c(mse_binary$n_not_sel, mse_ridge$n_not_sel, mse_ridge_cutoff001$n_not_sel, mse_ridge_cutoff05$n_not_sel, mse_ridge_cutoff1$n_not_sel),
    nwrongnotselected = c(mse_binary$n_wrong_not_sel, mse_ridge$n_wrong_not_sel, mse_ridge_cutoff001$n_wrong_not_sel, mse_ridge_cutoff05$n_wrong_not_sel, mse_ridge_cutoff1$n_wrong_not_sel)
  )
  }
  k = k+1
}
save(ll_rows, file = "ll_rows_cat_mses2.Rda")
load("ll_rows_cat_mses2.Rda")

df_cat_mses = do.call(rbind, ll_rows)

df_cat_mses$method = factor(df_cat_mses$method)
levels(df_cat_mses$method) = c("Binary", "Ridge", "Ridge (cutoff <0.01)", "Ridge (cutoff <0.5)", "Ridge (cutoff <1)")

df_bp = df_cat_mses %>%
  pivot_longer(cols = starts_with("mse")) %>%
  mutate(sn_ratiof = factor(paste0("SNR: ", sn_ratio), levels = paste0("SNR: ", c(0.1, 1, 10)))) %>%
  mutate(mse = factor(name))

levels(df_bp$mse) = c("MSE", "MSE of\nnoise classes", "MSE of\ninformative classes")

gg = df_bp %>%
  ggplot(aes(x = mse, y = value, fill = method, color = method)) +
    geom_boxplot(alpha = 0.2) +
    scale_fill_viridis(discrete = TRUE) +
    scale_color_viridis(discrete = TRUE) +
    xlab("") +
    ylab("MSE") +
    labs(fill = "", color = "", shape = "") +
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
    facet_grid(sn_ratiof ~ .) #, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "categorical_mse.pdf", width = dinA4width * 2/3 * 0.7, height = dinA4width * 2/3 * 0.5, units = "mm")

head(df_cat_mses)

df_plt_cat_mses = df_cat_mses %>%
  mutate(
    rel_notselected = nnotselected / nnoninfocls,
    rel_nwrongnotselected = nwrongnotselected / nnoninfocls
  ) %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses, sn_ratio) %>%
  summarize(
    rel_notselected = median(rel_notselected, na.rm = TRUE),
    rel_nwrongnotselected = median(rel_nwrongnotselected, na.rm = TRUE)
  ) %>%
  mutate(
    sn_ratiof = factor(paste0("SNR: ", sn_ratio), levels = paste0("SNR: ", c(0.1, 1, 10)))
  )

dim(df_plt_cat_mses)

#hull = df_cat_mses %>%
  #mutate(
    #rel_notselected = nnotselected / nnoninfocls,
    #rel_nwrongnotselected = nwrongnotselected / nnoninfocls,
    #sn_ratiof = factor(paste0("SNR: ", sn_ratio), levels = paste0("SNR: ", c(0.1, 1, 10)))
  #) %>%
  #filter(!is.nan(rel_nwrongnotselected), !is.nan(rel_notselected)) %>%
  #group_by(sn_ratiof, method) %>%
  #slice(chull(rel_nwrongnotselected, rel_notselected))


#hull = df_plt_cat_mses %>%
  filter(!is.nan(rel_nwrongnotselected), !is.nan(rel_notselected)) %>%
  #group_by(sn_ratiof, method) %>%
  #slice(chull(rel_nwrongnotselected, rel_notselected))

tmp = df_cat_mses %>%
  filter(method %in% c("Binary", "Ridge (cutoff <0.5)", "Ridge (cutoff <1)")) %>%
  mutate(
    rel_notselected = nnotselected / nnoninfocls,
    rel_nwrongnotselected = nwrongnotselected / nnoninfocls,
    sn_ratiof = factor(paste0("SNR: ", sn_ratio), levels = paste0("SNR: ", c(0.1, 1, 10)))
  ) %>%
  group_by(nrows, ncols, ncolsnoise, method, nclasses, sn_ratiof) %>%
  summarize(
    rel_notselected = median(rel_notselected, na.rm = TRUE),
    rel_nwrongnotselected = median(rel_nwrongnotselected, na.rm = TRUE)
  ) %>%
  select(rel_nwrongnotselected, rel_notselected, method, sn_ratiof) %>%
  filter(is.finite(rel_nwrongnotselected), is.finite(rel_notselected), rel_nwrongnotselected > 0, rel_notselected > 0) %>%
  na.omit()

ll_dens = list()
k = 1
for (m in unique(tmp$method)) {
  for (snr in unique(tmp$sn_ratiof)) {
    kd = ks::kde(tmp[(tmp$method == m) & (tmp$sn_ratiof == snr), c("rel_nwrongnotselected", "rel_notselected")],
           compute.cont=TRUE)
    kd = ks::kde(tmp[(tmp$method == m) & (tmp$sn_ratiof == snr), c("rel_nwrongnotselected", "rel_notselected")],
           compute.cont=TRUE, H = kd$H * 2)
    cont = with(kd, contourLines(x=eval.points[[1]], y=eval.points[[2]],
      z=estimate, levels=cont["5%"])[[1]], bgridsize=c(151,151))
    ll_dens[[k]] = data.frame(cont, method = m, sn_ratiof = snr)
    ll_dens[[k]]$rel_nwrongnotselected = ll_dens[[k]]$x
    ll_dens[[k]]$rel_notselected = ll_dens[[k]]$y
    k = k + 1
  }
}
df_dens = do.call(rbind, ll_dens)

gg = ggplot(mapping = aes(x = rel_nwrongnotselected, y = rel_notselected, shape = method, color = method, fill = method)) +
#ggplot(mapping = aes(x = rel_nwrongnotselected, y = rel_notselected, shape = method, color = method, fill = method)) +
  geom_polygon(data = df_dens, alpha = 0.2, size = 0.1) +
  geom_point(data = df_plt_cat_mses) +
  scale_fill_viridis(discrete = TRUE) +
  scale_color_viridis(discrete = TRUE) +
  xlab("Fraction of wrongly\nnot selected classes (FPR)") +
  ylab("Fraction of correctly\nnot selected classes (TPR)") +
  labs(fill = "", color = "", shape = "") +
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
  xlim(min(df_dens$rel_nwrongnotselected), max(df_dens$rel_nwrongnotselected)) +
  ylim(min(df_dens$rel_notselected), max(df_dens$rel_notselected)) +
  scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  facet_grid(sn_ratiof ~ .)#, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg, filename = "categorical_noninfo_count.pdf", width = dinA4width * 2/3 * 0.7, height = dinA4width * 2/3 * 0.5, units = "mm")



gg_cat_selected = df_cat_mses %>%
#df_cat_mses %>%
  group_by(nrows, method, nclasses, sn_ratio) %>%
  summarize(
    rel = median(nnotselected, na.rm = TRUE),
    min_rel = median(nnotselected, na.rm = TRUE) - sd(nnotselected, na.rm = TRUE),
    max_rel = median(nnotselected, na.rm = TRUE) + sd(nnotselected, na.rm = TRUE)
    #pn_rel = ncolsnoise[1] / ncols[1],
  ) %>%
  mutate(
    #pn_rel = ifelse(pn_rel == 0.4, 0.5, pn_rel),
    #ncolsf = factor(paste0("# p: ", ncols), levels = paste0("# p: ", c(5, 10, 20, 50))),
    sn_ratiof = factor(paste0("SNR: ", sn_ratio), levels = paste0("SNR: ", c(0.1, 1, 10)))
  ) %>%
  #filter(ncols == 50, nclasses == 20) %>%
    ggplot(aes(x = nrows, y = rel / nclasses * 2, linetype = as.factor(method), color = as.factor(nclasses))) +
      geom_linerange(aes(ymin = min_rel / nclasses * 2, ymax = max_rel / nclasses * 2), alpha = 0.5, position = position_dodge(width = 0.05)) +
      geom_line(position = position_dodge(width = 0.05)) +
      #scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Set1") +
      xlab("Number of rows\n(log10 Scale)") +
      ylab("Fraction of not selected\nnon-informative classes") +
      labs(linetype = "Method", fill = "Method", color = "Number of classes\nper feature") +
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
      scale_x_continuous(breaks = unique(df_cat_mses$nrows), trans = "log10") +
      facet_grid(sn_ratiof ~ .)#, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg_cat_selected, filename = "categorical_noninfo_count.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")



gg_cat_mse = df_cat_mses %>%
  group_by(nrows, ncols, ncolsnoise, nclasses, sn_ratio, rep, nnoninfocls) %>%
  summarize(
    pn_rel = ncolsnoise[1] / ncols[1],
    mse_diff = median(noninfo_mse[method == "ridge"], na.rm = TRUE) - median(noninfo_mse[method == "binary"], na.rm = TRUE)
  ) %>%
  mutate(pn_rel = ifelse(pn_rel == 0.4, 0.5, pn_rel)) %>%
  mutate(
    ncolsf = factor(paste0("# p: ", ncols), levels = paste0("# p: ", c(5, 10, 20, 50))),
    pn_relf = factor(paste0("rel p\nnoise:\n", pn_rel), levels = paste0("rel p\nnoise:\n", c(0.5, 1, 2, 5))),
    sn_ratiof = factor(paste0("SNR: ", sn_ratio), levels = paste0("SNR: ", c(0.1, 1, 10)))
  ) %>%
  group_by(nrows, ncolsf, pn_rel, nclasses, sn_ratiof, nnoninfocls) %>%
  summarize(
    mmse_diff = median(mse_diff),
    min_rel = median(mse_diff) - sd(mse_diff),
    max_rel = median(mse_diff) + sd(mse_diff)
  ) %>%
    ggplot(aes(x = nrows, y = mmse_diff,  linetype = as.factor(pn_rel), color = as.factor(nclasses))) +
      geom_linerange(aes(ymin = min_rel, ymax = max_rel), alpha = 0.5, position = position_dodge(width = 0.05)) +
      geom_line(position = position_dodge(width = 0.05)) +
      #geom_smooth(se = FALSE) +
      #scale_fill_viridis(discrete = TRUE) +
      #scale_color_viridis(discrete = TRUE) +
      scale_color_brewer(palette = "Set1") +
      xlab("Number of Rows\n(log10 Scale)") +
      ylab("MSE(Ridge) - MSE(Binary)\nof Non-Informative Classes") +
      labs(linetype = "Relative number\nof noise classes", fill = "Method", color = "# Classes") +
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
      scale_x_continuous(breaks = unique(df_cat_mses$nrows), trans = "log10") +
      facet_grid(sn_ratiof ~ ncolsf, scales = "free_y")

dinA4width = 210 * font_scale
ggsave(plot = gg_cat_mse, filename = "categorical_noninfo_mse.pdf", width = dinA4width * 2/3, height = dinA4width * 2/3 * 0.6, units = "mm")





# Calculate MSE of estimated parameter:
# ---------------------------------------------------

files = list.files("performance", full.names = TRUE)
files = files[grep("xxx", files)]

ll_rows = list()
k = 1

for (fn in files) {
  cat(as.character(Sys.time()), "Read: ", k , "/", length(files), "\n")

  load(fn)

  set.seed(bm_extract$data_seed)
  dat = simCategoricalData(bm_extract$config$n, bm_extract$config$p, bm_extract$config$pnoise, nclasses = bm_extract$config_classes$ncls[1], ncnoise = bm_extract$config_classes$nic[1])

  real_params = dat$cat_param

  est_params = bm_extract$coef_ridge[[2]]
  names(est_params) = vapply(names(est_params), FUN.VALUE = character(1L), FUN = function (nm) strsplit(nm, split = "_")[[1]][1])
  est_params = transformRidgeToParam(est_params, dat$data)
  mse_ridge = getCategoricalMSE(real_params, est_params, TRUE)

  est_params = bm_extract$coef_binary[[2]]
  est_params = transformBinaryToParam(est_params)
  mse_binary = getCategoricalMSE(real_params, est_params, TRUE)

  ll_rows[[k]] = data.frame(
    date        = bm_extract$date,
    data_seed   = bm_extract$data_seed,
    nrows       = bm_extract$config$n,
    ncols       = bm_extract$config$p,
    sn_ratio    = bm_extract$config$sn_ratio,
    rep         = bm_extract$config$rep,
    ncolsnoise  = bm_extract$config$pnoise,
    nclasses    = bm_extract$config_classes$ncls[1],
    nnoninfocls = bm_extract$config_classes["nic"][1,1],
    time_init   = c(bm_extract$time_binary["init.elapsed"], bm_extract$time_ridge["init.elapsed"]),
    time_fit   = c(bm_extract$time_binary["fit.elapsed"], bm_extract$time_ridge["fit.elapsed"]),
    method      = c("binary", "ridge"),
    mse  = c(mse_binary, mse_ridge)
  )
  k = k+1
}
save(ll_rows, file = "ll_rows_cat_mses_full.Rda")
load("ll_rows_cat_mses_full.Rda")

df_mses = do.call(rbind, ll_rows)

df_mses %>%
  filter(mse < 300) %>%
  group_by(nrows, ncols, sn_ratio, rep, ncolsnoise, nclasses, nnoninfocls) %>%
  mutate(
    pn_rel = ncolsnoise[1] / ncols[1],
    clsn_rel = nnoninfocls[1] / nclasses[1],
    mse_diff = median(mse[method == "ridge"], na.rm = TRUE) - median(mse[method == "binary"], na.rm = TRUE)
  ) %>%
  mutate(
    pn_rel = ifelse(pn_rel == 0.4, 0.5, pn_rel),
    clsn_rel = ifelse(clsn_rel == 0.4, 0.5, clsn_rel)
  ) %>%
  ggplot(aes(x = as.factor(nrows), y = mse_diff, fill = as.factor(pn_rel))) +
    geom_boxplot() +
    scale_fill_viridis(discrete = TRUE) +
    #scale_color_viridis(discrete = TRUE) +
    #scale_fill_brewer(palette = "Set1") +
    #scale_color_brewer(palette = "Set1") +
    xlab("Number of Rows\n(log10 Scale)") +
    ylab("MSE(Ridge) - MSE(Binary)\nof Non-Informative Classes") +
    labs(linetype = "Relative number\nof noise classes", fill = "Method", color = "# Classes") +
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
    #facet_grid(sn_ratio + nnoninfocls ~ ncols + ncolsnoise, scales = "free_y")
    facet_grid(paste0("SNR: ", sn_ratio) ~ nclasses, scales = "free_y")

