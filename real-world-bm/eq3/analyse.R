## PACKAGES:
## =============================================

library(ggplot2)
library(dplyr)
library(tidyr)
library(gridExtra)

library(batchtools)

loadRegistry(here::here("real-world-bm/eq3/batchtools"))

## FIGURE SETTINGS:
## =============================================

if (FALSE) {
  font = "TeX Gyre Bonum"

  sysfonts::font_add(font,
      regular = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-regular.otf",
      bold = "/usr/share/texmf-dist/fonts/opentype/public/tex-gyre/texgyrebonum-bold.otf")

  extrafont::font_import(paths = "~/repos/bm-CompAspCboost/paper-figures/gyre-bonum", prompt = FALSE)
  extrafont::loadfonts()
}


theme_set(
  #theme_minimal(base_family = font) +
  theme_minimal() +
  ggplot2::theme(
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


## LOAD RESULTS:
## =============================================

load(here::here("real-world-bm/eq3/results-summarized/df_res.Rda"))

## PROCESS RESULTS:
## =============================================

lids = c(
  "acc_acwb_b" = "ACWB (b)",
  "acc_hcwb_b" = "hCWB (b)",
  "ebm"        = "EBM",
  "xgboost"    = "XGBoost")

df_bmr = df_res %>%
  mutate(learner = factor(learner), test_auc = auc_test, seconds = job_time_running, task = as.factor(task)) %>%
  select(task, fold, learner, test_auc, seconds)

levels(df_bmr$learner) = lids[levels(df_bmr$learner)]


## EQ3:
## =============================================

## Figure 7:
## ---------------------------------------------

df_bmr_smr = df_bmr %>%
  group_by(learner, task) %>%
  mutate(seconds = seconds / 60^2) %>%
  summarize(auc = mean(test_auc, na.rm = TRUE), sec = mean(seconds, na.rm = TRUE),
    sd_auc = sd(test_auc, na.rm = TRUE), sd_sec = sd(seconds, na.rm = TRUE),
    auc_min = min(test_auc), auc_max = max(test_auc), auc25 = quantile(test_auc, 0.25), auc75 = quantile(test_auc, 0.75),
    sec_min = min(seconds), sec_max = max(seconds), sec25 = quantile(seconds, 0.25), sec75 = quantile(seconds, 0.75)) %>%
  ungroup() %>%
  mutate(learner = factor(learner))

tdims = sapply(levels(df_bmr_smr$task), function(tn) {
  ts = TASKS[[tn]]
  fl = length(ts$feature_names)
  if (tn == "spam") tnn = "Spam"
  if (tn == "168908") tnn = "Christine"
  if (tn == "9977") { tnn = "Namao"; fl = 112 }
  if (tn == "7592") tnn = "Adult"
  if (tn == "168335") tnn = "MiniBooNE"
  if (tn == "189866") tnn = "Albert"
  return(paste0(tnn, "   n: ", ts$nrow, ", p: ", fl))
})

df_bmr_smr$task = factor(tdims[df_bmr_smr$task])
df_bmr_smr$task = factor(df_bmr_smr$task, levels = levels(df_bmr_smr$task)[c(6, 3, 5, 1, 4, 2)])

mycolors = ggsci::pal_uchicago()(6)[c(5:6, 1:2)]

gg_bb = ggplot(df_bmr_smr) +
  geom_segment(aes(y = auc_min, x = sec, yend = auc, xend = sec, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec_min, yend = auc, xend = sec_max, color = learner), size = 0.2, alpha = 0.8) +
  geom_segment(aes(y = auc25, x = sec, yend = auc75, xend = sec, color = learner), size = 1., alpha = 0.8) +
  geom_segment(aes(y = auc, x = sec25, yend = auc, xend = sec75, color = learner), size = 1., alpha = 0.8) +
  geom_point(aes(x = sec, y = auc), color = "white", size = 3) +
  geom_point(aes(x = sec, y = auc, color = learner), size = 2) +
  scale_fill_manual(values = mycolors) +
  scale_color_manual(values = mycolors) +
  labs(shape = "") +
  xlab("Tuning time (hours)") +
  ylab("Test AUC") +
  labs(color = "Learner") +
  scale_y_continuous(breaks = scales::pretty_breaks(4)) +
  scale_x_continuous(breaks = scales::pretty_breaks(4)) +
  facet_wrap(. ~ task, scales = "free")

#ggsave(gg_bb, filename = here::here("real-world-bm/eq3/figures/fig-eq3.pdf"), width = dinA4width * 1.15, height = dinA4width * 0.5, units = "mm")


## average improvements and total numbers:
## -----------------------------------------

df_bmr_vals = df_bmr %>%
  #group_by(task, learner, fold) %>%
  group_by(task, learner) %>%
  summarize(seconds = mean(seconds), auc = mean(test_auc)) %>%
  group_by(task) %>%
  summarize(
    learner       = learner,
    rel_secs_acwb = seconds / seconds[learner == "ACWB (b)"],
    rel_secs_hcwb = seconds / seconds[learner == "hCWB (b)"],
    rel_auc_acwb  = auc / auc[learner == "ACWB (b)"] - 1,
    rel_auc_hcwb  = auc / auc[learner == "hCWB (b)"] - 1)

tdims_vals = sapply(levels(df_bmr_vals$task), function(tn) {
  if (tn == "spam") tnn = "Spam"
  if (tn == "168908") tnn = "Christine"
  if (tn == "9977") tnn = "Namao"
  if (tn == "7592") tnn = "Adult"
  if (tn == "168335") tnn = "MiniBooNE"
  if (tn == "189866") tnn = "Albert"
  return(tnn)
})

df_bmr_vals$task = factor(tdims[df_bmr_vals$task])
df_bmr_vals$task = factor(df_bmr_vals$task, levels = levels(df_bmr_vals$task)[c(6, 3, 5, 1, 4, 2)])

## Relative AUC:
df_bmr_vals %>%
  mutate(rel_auc = (rel_auc_acwb + rel_auc_hcwb) / 2) %>%
  select(task, learner, rel_auc)  %>%
  pivot_wider(names_from = task, values_from = rel_auc) %>%
  filter(! grepl("CWB", learner))

## Relative runtime:
df_bmr_vals %>% group_by(learner) %>%
  summarize(
    rel_secs_acwb = mean(rel_secs_acwb),
    rel_secs_hcwb = mean(rel_secs_hcwb)) %>%
  mutate(rel_secs = (rel_secs_acwb + rel_secs_hcwb) / 2) %>%
  as.data.frame()

df_bmr_vals %>% group_by(learner, task) %>%
  summarize(
    rel_secs_acwb = mean(rel_secs_acwb),
    rel_secs_hcwb = mean(rel_secs_hcwb)) %>%
  mutate(rel_secs = (rel_secs_acwb + rel_secs_hcwb) / 2) %>%
  as.data.frame()

df_bmr %>% group_by(learner) %>% summarize(time = sum(seconds) /  60^2)
df_bmr %>%
  group_by(learner, task) %>%
  summarize(time = 5 * mean(seconds) /  60^2) %>%
  group_by(learner) %>%
  summarize(time = sum(time))


## Create benchmark table:
## ----------------------------------

df_bmr_full = expand.grid(task = tdims, learner = levels(df_bmr$learner))

nround = 3
df_bmr_full = rbind(
  df_bmr_full %>%
    left_join(df_bmr_smr %>% select(task, learner, auc, sd_auc)) %>%
    mutate(value = paste0("$", round(auc, nround), " \\pm ", round(sd_auc, nround), "$"), auc = NULL, sd_auc = NULL) %>%
    mutate(measure = "auc") %>%
    pivot_wider(names_from = task),
  df_bmr_full %>%
    left_join(df_bmr_smr %>% select(task, learner, sec, sd_sec)) %>%
    mutate(value = paste0("$", round(sec, nround), " \\pm ", round(sd_sec, nround), "$"), sec = NULL, sd_sec = NULL) %>%
    mutate(measure = "time") %>%
    pivot_wider(names_from = task)
)

a = knitr::kable(df_bmr_full, format = "latex", escape = FALSE)
lines = strsplit(a, "\n")[[1]]

idx_head = grep("learner", lines)
lines = lines[-seq_len(idx_head)]

idx_hline = grep("hline", lines)
lines = lines[-idx_hline]

lines = lines[-length(lines)]

idx_auc = grep("auc", lines)
idx_time = grep("time", lines)

lines = gsub("& auc ", "", lines, fixed = TRUE)
lines = gsub("& time ", "", lines, fixed = TRUE)

ncols = length(names(df_bmr_full))
lines_header = c("\\begin{tabular}{|ll|c|c|c|c|c|c|}",
  "\\hline",
  "& \\multirow{2}{*}{\\textbf{Algorithm}} & \\multicolumn{6}{c|}{\\textbf{Data set}} \\\\",
  paste0(" & & ", paste(paste0("\\textbf{", names(df_bmr_full)[-(1:2)], "}"), collapse = " & "), "\\\\"),
  "\\hline\\hline")

lines[c(idx_auc[1], idx_time[1])] = paste0(
  c(paste0("\\multirow{", length(idx_auc), "}{*}{AUC} & "),
    paste0("\\multirow{", length(idx_time), ",}{*}{\\makecell{Runtime \\\\ (in hours)}} & ")),
  lines[c(idx_auc[1], idx_time[1])])
lines[c(idx_auc[-1], idx_time[-1])] = paste0(paste0("\\cline{2-", ncols, "}\n & "), lines[c(idx_auc[-1], idx_time[-1])])

lines_tail = c("\\hline", "\\end{tabular}\n")

lines_final = paste0("\n", c(
  lines_header,
  lines[idx_auc],
  "\\hline\\hline",
  lines[idx_time],
  lines_tail
))
cat(lines_final)
