## PACKAGES:
## =============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(batchtools)
library(mlr3)
library(gridExtra)

## FIGURE SETTINGS:
## =============================================

# Load TeX Gyre Bonum to replicate the used font:
if (FALSE) {
  font = "TeX Gyre Bonum"

  sysfonts::font_add(font,
      regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
      bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")
  #showtext::showtext_auto()
  extrafont::font_import(paths = "~/repos/bm-CompAspCboost/paper-figures/gyre-bonum", prompt = FALSE)
  extrafont::loadfonts()
}
theme_set(
  theme_minimal() +
  #theme_minimal(base_family = font) +
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
mycolors = ggsci::pal_uchicago()(6)[4:6]


## HELPER:
## =============================================

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
      return(paste0(tnn, "   n: ", ts$nrow, ", p: ", length(ts$feature_names)))
    } else {
      return(tnn)
    }
  })
}

## GET RESULTS:
## =============================================

base_dir = here::here("real-world-bm/eq1-2/")
bm_dir   = paste0(base_dir, "batchtools/")
fig_dir  = function(fig_name) paste0(base_dir, "/figures/", fig_name)

loadRegistry(file.dir = bm_dir, work.dir = base_dir)
getStatus()

jt  = getJobTable(findDone())
res = reduceResultsList(jt)

## PROCESS RESULTS:
## =============================================

for (i in seq_along(res)) {
  res[[i]] = cbind(res[[i]],
    fold = as.numeric(gsub("\\D", "", jt$problem[i])),
    task = sub("\\-.*", "", jt$problem[i]),
    learner = jt$algo.pars[[i]]$lid)
  res[[i]][["_iterations"]] = seq_len(nrow(res[[i]]))

  # In compboost, the AUC is defined as 1 - AUC since
  # stopping is done on minimization. Hence, transform
  # the AUC back:
  if ("riskauc" %in% names(res[[i]]))
    res[[i]]$test_auc  = res[[i]]$riskauc

  res[[i]]$test_auc = 1 - res[[i]]$test_auc
  res[[i]]$val_auc  = 1 - res[[i]]$val_auc
  res[[i]]$seconds  = res[[i]]$time / 1e6
}
df_res = do.call(rbind, lapply(res, function(r) {
  lnames = c("_iterations", "test_auc", "seconds", "blearner", "risk", "model", "transition", "fold", "task", "learner")
  r[, lnames]
}))
df_res[["iteration"]]    = df_res[["_iterations"]]
df_res[["_iterations"]]  = NULL
df_res[["test_auc"]]     = df_res[["test_auc"]]
df_res[["task"]]         = factor(df_res[["task"]], levels = TSKS_SETUP$id)

lids_old = c("cwb", "cwb_b", "acwb", "hcwb", "hcwb_b", "acwb_b")
lids_new = c("CWB", "CWB (b)", "ACWB", "ACWB (b)", "hCWB", "hCWB (b)")

lids = c(
  "cwb"    = "CWB",
  "cwb_b"  = "CWB (b)",
  "acwb"   = "ACWB",
  "acwb_b" = "ACWB (b)",
  "hcwb"   = "hCWB",
  "hcwb_b" = "hCWB (b)")

df_res = df_res %>%
  filter(learner != "acc_hcwb2") %>%
  mutate(learner = factor(learner))# %>%
  #filter(! learner %in% c("CWB (b)", "CWB (nb)"))

levels(df_res$learner) = lids[levels(df_res$learner)]


## SUMMARY VALUES:
## =============================================

df_smry = df_res %>%
  group_by(task, learner, iteration) %>%
  summarize(test_auc = mean(test_auc),
    seconds    = mean(seconds),
    transition = transition[1]) %>%
  group_by(task, learner) %>%
  summarize(
    auc_start  = min(test_auc),
    iter_best  = iteration[which.max(test_auc)],
    auc_best   = max(test_auc),
    sec_best    = seconds[which.max(test_auc)],
    transition = transition[1],
    auc_expl   = (max(test_auc) - min(test_auc)) / min(test_auc)
  ) %>%
  group_by(task) %>%
  mutate(auc_start = min(auc_start)) %>%
  group_by(task) %>%
  mutate(
    auc_diff_to_cwb = ifelse("CWB" %in% learner, auc_best[learner == "CWB"] - auc_best, NA),
    binning = ifelse(grepl("(b)", learner), "Binning", "No binning"),
    learner = gsub(" (nb)", "", gsub(" (b)", "", learner, fixed = TRUE), fixed = TRUE))

df_smry$learner = factor(df_smry$learner, levels = c("CWB", "ACWB", "hCWB"))
df_smry$binning = factor(df_smry$binning, levels = c("No binning", "Binning"))


## EQ2:
## =============================================

## Figure 5:
## ---------------------------------------------

df_aggr = df_res %>%
  group_by(task, learner, iteration) %>%
  summarize(test_auc = mean(test_auc), seconds = mean(seconds))

df_aggr2 = df_aggr %>%
  mutate(
    binning = ifelse(grepl("(b)", learner), "Binning", "No binning"),
    learner = gsub(" (nb)", "", gsub(" (b)", "", learner, fixed = TRUE), fixed = TRUE))

df_aggr2$learner[df_aggr2$learner == "hCWB"] = "HCWB"
df_aggr2$learner = factor(df_aggr2$learner, levels = c("CWB", "ACWB", "HCWB"))
df_aggr2$binning = factor(df_aggr2$binning, levels = c("No binning", "Binning"))

tdims = taskIDtoName(levels(df_aggr2$task), TASKS)
df_aggr2$taskn = factor(tdims[df_aggr2$task])
df_aggr2$taskn = factor(df_aggr2$taskn, levels = levels(df_aggr2$taskn)[c(6, 3, 5, 1, 4, 2)])

df_smry$taskn = factor(tdims[df_smry$task])
df_smry$taskn = factor(df_smry$taskn, levels = levels(df_smry$taskn)[c(6, 3, 5, 1, 4, 2)])


gg_tr = ggplot() +
  geom_line(data = df_aggr2, mapping = aes(x = seconds, y = test_auc, color = learner, linetype = binning), size = 0.5, alpha = 0.8) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  xlab("Training time (seconds, square root scale)") +
  ylab("Test AUC") +
  scale_x_continuous(trans = "sqrt") +#, guide = guide_axis(n.dodge = 2)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "Learner", linetype = "", shape = "") +
  facet_wrap(. ~ taskn, nrow = 2, scales = "free")

#ggsave(gg_tr, filename = fig_dir("fig-eq2-1.pdf"), width = dinA4width * 1.15, height = dinA4width * 0.5, units = "mm")


## Figure 6:
## ---------------------------------------------

df_stop           = do.call(rbind, lapply(res, getStopInfo))
df_stop[["task"]] = factor(df_stop[["task"]], levels = TSKS_SETUP$id)

df_stop_smr = df_stop %>%
  #filter(learner != "acc_hcwb2") %>%
  group_by(learner, task) %>%
  summarize(auc = median(test_auc), sec = median(seconds),
    auc_min = min(test_auc), auc_max = max(test_auc), auc25 = quantile(test_auc, 0.25), auc75 = quantile(test_auc, 0.75),
    sec_min = min(seconds), sec_max = max(seconds), sec25 = quantile(seconds, 0.25), sec75 = quantile(seconds, 0.75)) %>%
  ungroup() %>%
  mutate(learner = factor(learner))
levels(df_stop_smr$learner) = lids[levels(df_stop_smr$learner)]
df_stop_smr$learner = factor(df_stop_smr$learner, levels = lids)

df_stop_smr = df_stop_smr %>%
  mutate(
    binning = ifelse(grepl("(b)", learner), "Binning", "No binning"),
    learner = gsub(" (nb)", "", gsub(" (b)", "", learner, fixed = TRUE), fixed = TRUE))

df_stop_smr$learner[df_stop_smr$learner == "hCWB"] = "HCWB"
df_stop_smr$learner = factor(df_stop_smr$learner, levels = c("CWB", "ACWB", "HCWB"))
df_stop_smr$binning = factor(df_stop_smr$binning, levels = c("No binning", "Binning"))

tdims = taskIDtoName(levels(df_stop_smr$task), TASKS)
df_stop_smr$task = factor(tdims[df_stop_smr$task])
df_stop_smr$task = factor(df_stop_smr$task, levels = levels(df_stop_smr$task)[c(6, 3, 5, 1, 4, 2)])

gg_bb = ggplot(df_stop_smr) +
  geom_rect(data = df_stop_smr %>% filter(learner == "CWB", binning == "No binning"),
    aes(xmin = -Inf, xmax = Inf, ymin = auc25, ymax = auc75),
    alpha = 0.1, size = 0, show.legend = FALSE, fill = mycolors[1]) +
  geom_rect(data = df_stop_smr %>% filter(learner == "CWB", binning == "No binning"),
    aes(xmin = sec25, xmax = sec75, ymin = -Inf, ymax = Inf),
    alpha = 0.1, size = 0, show.legend = FALSE, fill = mycolors[1]) +
  geom_segment(aes(y = auc_min, x = sec, yend = auc, xend = sec, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec_min, yend = auc, xend = sec_max, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc25, x = sec, yend = auc75, xend = sec, color = learner), size = 1., alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec25, yend = auc, xend = sec75, color = learner), size = 1., alpha = 0.8) +
  geom_point(aes(x = sec, y = auc, shape = binning), color = "white", size = 3) +
  geom_point(aes(x = sec, y = auc, color = learner, shape = binning), size = 2) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(shape = "") +
  xlab("Training time (seconds)") +
  ylab("Test AUC") +
  labs(color = "Learner") +
  facet_wrap(. ~ task, scales = "free")

#ggsave(gg_bb, filename = fig_dir("fig-eq2-2.pdf"), width = dinA4width * 1.15, height = dinA4width * 0.5, units = "mm")


## Relative runtime improvement and AUC difference:
## --------------------------------------------------------

ntsks = c("spam" = "Spam", "168908" = "Christine", "7592" = "Adult",
  "168335" = "MiniBooNE", "189866" = "Albert", "9977" = "Namao")

df_smry2 = df_stop_smr %>%
  mutate(task = names(task)) %>%
  group_by(task) %>%
  mutate(
    speedup = sec[(learner == "CWB") & (binning == "No binning")] / sec,
    auc_imp = auc - auc[(learner == "CWB") & (binning == "No binning")])

df_smry2 %>%
  filter(! ((learner == "CWB") & (binning == "No binning"))) %>%
  group_by(learner, binning) %>%
  summarize(mspeedup = mean(speedup))

df_smry31 = df_smry2 %>%
  filter(! ((learner == "CWB") & (binning == "No binning"))) %>%
  mutate(taskn = ntsks[task]) %>%
  group_by(taskn, learner, binning) %>%
  summarize(mspeedup = mean(speedup)) %>%
  pivot_wider(names_from = taskn, values_from = mspeedup, names_prefix = "sec-")

df_smry32 = df_smry2 %>%
  filter(! ((learner == "CWB") & (binning == "No binning"))) %>%
  mutate(taskn = ntsks[task]) %>%
  group_by(taskn, learner, binning) %>%
  summarize(mauc_imp = mean(auc_imp)) %>%
  pivot_wider(names_from = taskn, values_from = mauc_imp, names_prefix = "auc-")

df_smry3 = cbind(df_smry31, df_smry32[,-c(1,2)])
pres = c("sec-", "auc-")
knitr::kable(df_smry3[, c("learner", "binning",
  paste0(pres, "Spam"),
  paste0(pres, "Christine"),
  paste0(pres, "Namao"),
  paste0(pres, "Adult"),
  paste0(pres, "MiniBooNE"),
  paste0(pres, "Albert"))])


## Beta regression for AUC values:
## -------------------------------------------

library(mgcv)

df_smry$learner = factor(df_smry$learner, levels = c("CWB", "ACWB", "hCWB"))
df_smry$binning = factor(df_smry$binning, levels = c("No binning", "Binning"))

mod = gam(auc_best ~ task + binning + learner + binning*learner, data = df_smry, family = betar)

knitr::kable(summary(mod)$p.table)
knitr::kable(summary(mod)$pTerms.table)



## EQ1:
## =============================================

cwb_stops = df_stop %>% filter(learner == "cwb")
substrRight = function(x, n = 1) substr(x, nchar(x) - n + 1, nchar(x))

ll_tt   = list()
idx_run = seq_len(nrow(cwb_stops))

dffile = here::here("real-world-bm/eq1-2/meta/df_mboost.Rda")

if (FALSE) {
if (file.exists(dffile)) {
  load(dffile)
  for (i in seq_len(nrow(df_mboost))) {
    ll_tt[[i]] = df_mboost[i,]
  }
  idx_run = which(is.na(df_mboost$train_time))
}

errs = c()
for (i in idx_run) {
  message("[", Sys.time(), "] ", i, "/", nrow(cwb_stops))
  l = lrn("classif.gamboost", mstop = cwb_stops$stop[i])
  fold = as.integer(substrRight(cwb_stops$fold[i]))
  tset = RESAMPLE_SETS[[cwb_stops$task[i]]]$train_set(fold)

  ts = TASKS[[cwb_stops$task[i]]]$clone(deep = TRUE)$filter(tset)
  if (as.character(cwb_stops$task[i]) == "9977") {
    feats_remove = paste0("V", c(17:18, 41:42, 73:74, 81:86))
    ts = ts$select(setdiff(ts$feature_names, feats_remove))
  }


  ## DETECT FEATURES WHO CRASH THE TRAINING
  #fnames = ts$feature_names
  #for (j in seq_along(fnames)) {
  #  tss = ts$clone(deep = TRUE)$select(fnames[j])

  #  robustify = po("removeconstants", id = "removeconstants_before") %>>%
  #    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
  #    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
  #    po("collapsefactors", target_level_count = 10) %>>%
  #    po("removeconstants", id = "removeconstants_after")
  #  tss = robustify$train(tss)
  #
  #  l = lrn("classif.gamboost", mstop = 1)
  #  e = try(l$train(tss[[1]]), silent = TRUE)
  #  if (inherits(e, "try-error")) message("ERRROR: ", fnames[j])
  #}

  robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")
  ts = robustify$train(ts)

  e = NULL
  e = try({ l$train(ts[[1]]); TRUE }, silent = TRUE)
  if (inherits(e, "try-error")) {
    ll_tt[[i]] = data.frame(learner = "mboost", train_time = NA,
      task = cwb_stops$task[i], fold = fold, mstop = cwb_stops$stop[i])
    errs = c(errs, paste0("ERROR: ", attr(e, "condition")$message))
    msg  = last(errs)
  } else {
    ll_tt[[i]] = data.frame(learner = "mboost", train_time = l$state$train_time,
      task = cwb_stops$task[i], fold = fold, mstop = cwb_stops$stop[i])
    msg = paste0("FINISH training model in ", l$state$train_time, " seconds")
  }
  message("[", Sys.time(), "] ", i, "/", nrow(cwb_stops), ": ", msg)
}
df_mboost = do.call(rbind, ll_tt)
save(df_mboost, file = "df_mboost.Rda")
}

load(dffile)

df_mb = df_mboost %>%
  rbind(df_stop %>% select(learner, train_time = seconds, task, fold, mstop = stop)) %>%
  mutate(fold = substrRight(fold), time = train_time, train_time = NULL, task = taskIDtoName(task, TASKS)) %>%
  group_by(task) %>%
  mutate(speedup = time[learner == "mboost"] / time,
    binning = ifelse(grepl("_b", learner), "Binning", "No binning"),
    learner = ifelse(grepl("hcwb", learner), "HCWB", ifelse(grepl("acwb", learner), "ACWB", "CWB")))

df_mb$learner = factor(df_mb$learner, levels = c("CWB", "ACWB", "HCWB"))
df_mb$task    = factor(df_mb$task, levels = unique(df_mb$task)[c(1, 5, 6, 2, 3, 4)])
df_mb$binning = factor(df_mb$binning, levels = c("No binning", "Binning"))

gg_mboost = ggplot(df_mb %>% filter(learner != "mboost", ! grepl("Christine", task)), aes(x = learner, y = speedup, color = learner, fill = learner, linetype = binning)) +
  geom_hline(yintercept = 1, color = "dark red", linetype = "dashed", alpha = 0.7) +
  geom_boxplot(alpha = 0.4, size = 0.2) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(color = "Learner", fill = "Learner", linetype = "") +
  xlab("") +
  ylab("Speedup") +
  theme(axis.text.x = element_blank(), legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0), legend.box.margin = margin(-10, -10, 5, -10),
    strip.text = element_text(color = "black", face = "bold", size = 4)) +
  facet_wrap(. ~ task, scales = "free", ncol = 5)

#ggsave(gg_mboost, filename = fig_dir("fig-eq1.pdf"), width = dinA4width * 1, height = dinA4width * 0.25, units = "mm")

df_mb %>%
  group_by(task, learner) %>%
  summarize(speedup = mean(speedup, na.rm = TRUE), time = mean(time, na.rm = TRUE)) %>%
  as.data.frame()


