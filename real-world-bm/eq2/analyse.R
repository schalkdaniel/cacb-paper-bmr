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

# Variable if ggplots should be saved:
SAVEGG = FALSE

theme_set(
  theme_minimal() +
  ggplot2::theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(color = "black", face = "bold", size = 10),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 11),
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    panel.border = element_rect(colour = "black", fill = NA, size = 0.5)
  )
)
DINA4WIDTH = 162
MYCOLORS = ggsci::pal_uchicago()(6)[4:6]

# important dirs:
BASE_DIR = here::here("real-world-bm/eq2/")
BT_DIR   = paste0(BASE_DIR, "batchtools/")
FIG_DIR  = function(fig_name) paste0(BASE_DIR, "/figures/", fig_name)


## HELPER:
## =============================================

source(paste0(BASE_DIR, "R/helper.R"))

## GET RESULTS:
## =============================================

# Load result registry:
loadRegistry(file.dir = BT_DIR, work.dir = BASE_DIR)
getStatus()

# Get results as list:
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

# Aggregate results:
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
  scale_fill_manual(values = MYCOLORS) +
  scale_color_manual(values = MYCOLORS) +
  xlab("Training time (seconds, square root scale)") +
  ylab("Test AUC") +
  scale_x_continuous(trans = "sqrt", breaks = scales::pretty_breaks(4L)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(color = "Learner", linetype = "", shape = "") +
  facet_wrap(. ~ taskn, nrow = 2, scales = "free")

if (SAVEGG) {
  ggsave(gg_tr,
    filename = FIG_DIR("fig-eq2-1.pdf"),
    width = DINA4WIDTH,
    height = DINA4WIDTH * 0.5,
    units = "mm")
}


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
    alpha = 0.1, size = 0, show.legend = FALSE, fill = MYCOLORS[1]) +
  geom_rect(data = df_stop_smr %>% filter(learner == "CWB", binning == "No binning"),
    aes(xmin = sec25, xmax = sec75, ymin = -Inf, ymax = Inf),
    alpha = 0.1, size = 0, show.legend = FALSE, fill = MYCOLORS[1]) +
  geom_segment(aes(y = auc_min, x = sec, yend = auc, xend = sec, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec_min, yend = auc, xend = sec_max, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc25, x = sec, yend = auc75, xend = sec, color = learner), size = 1., alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec25, yend = auc, xend = sec75, color = learner), size = 1., alpha = 0.8) +
  geom_point(aes(x = sec, y = auc, shape = binning), color = "white", size = 3) +
  geom_point(aes(x = sec, y = auc, color = learner, shape = binning), size = 2) +
  scale_fill_manual(values = MYCOLORS) +
  scale_color_manual(values = MYCOLORS) +
  labs(shape = "") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(breaks = scales::pretty_breaks(4L)) +
  xlab("Training time (seconds)") +
  ylab("Test AUC") +
  labs(color = "Learner") +
  facet_wrap(. ~ task, scales = "free")

if (SAVEGG) {
  ggsave(gg_bb,
    filename = FIG_DIR("fig-eq2-2.pdf"),
    width = DINA4WIDTH,
    height = DINA4WIDTH * 0.5,
    units = "mm")
}


## Relative runtime improvement and AUC difference:
## --------------------------------------------------------

# This code creates the tables.

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

