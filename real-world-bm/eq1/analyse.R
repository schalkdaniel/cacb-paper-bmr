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

# ggplot settings:
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

# Gained from \printinunitsof{cm}\prntlen{\textwidth} in the latex doc
DINA4WIDTH = 162
MYCOLORS = ggsci::pal_uchicago()(6)[4:6]

# Important dirs:
BASE_DIR = here::here("real-world-bm/eq1/")
FIG_DIR  = function(fig_name) paste0(BASE_DIR, "/figures/", fig_name)

# We use the results from EQ2 to get the best stopping iterations also
# used for mboost:
EQ2_DIR  = here::here("real-world-bm/eq2")
BT_DIR   = paste0(EQ2_DIR, "/batchtools")

## HELPER:
## =============================================

source(paste0(BASE_DIR, "R/helper.R"))


## GET RESULTS:
## =============================================

# Load benchmark registry and check status. When loading the
# registry, meta data such as the tasks (in `TASKS`) is loaded:
loadRegistry(file.dir = BT_DIR, work.dir = here::here("real-world-bm/eq2"))


## EQ1:
## =============================================

# File with results (df_mboost.Rda) and stop
# information from compboost (df_stop.Rda):
load(paste0(BASE_DIR, "meta/df_mboost.Rda"))
load(paste0(BASE_DIR, "meta/df_stop.Rda"))

# Processing:
# - Get the relevant features
# - Calculate speedup
# - Prettify names
df_mb = df_mboost %>%
  rbind(df_stop %>%
    select(learner, train_time = seconds, task, fold, mstop = stop)) %>%
  mutate(
    fold = substrRight(fold),
    time = train_time,
    train_time = NULL,
    task = taskIDtoName(task, TASKS)) %>%
  group_by(task) %>%
  mutate(speedup = time[learner == "mboost"] / time,
    binning = ifelse(grepl("_b", learner), "Binning", "No binning"),
    learner = ifelse(
      grepl("hcwb", learner),
      "HCWB",
      ifelse(grepl("acwb", learner), "ACWB", "CWB")))

df_mb$learner = factor(df_mb$learner, levels = c("CWB", "ACWB", "HCWB"))
df_mb$task    = factor(df_mb$task, levels = unique(df_mb$task)[c(1, 5, 6, 2, 3, 4)])

# Rearrange levels to sort them by data size:
df_mb$binning = factor(df_mb$binning, levels = c("No binning", "Binning"))

gg_mboost = ggplot(df_mb %>%
    filter(learner != "mboost", ! grepl("Christine", task)),
    aes(x = learner, y = speedup, color = learner, fill = learner,
      linetype = binning)) +
  geom_hline(yintercept = 1, color = "dark red", linetype = "dashed",
    alpha = 0.7) +
  geom_boxplot(alpha = 0.4, size = 0.2) +
  scale_fill_manual(values = MYCOLORS) +
  scale_color_manual(values = MYCOLORS) +
  labs(color = "Learner", fill = "Learner", linetype = "") +
  xlab("") +
  ylab("Speedup") +
  theme(axis.text.x = element_blank(),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0),
    legend.box.margin = margin(-10, -10, 5, -10)) +
  facet_wrap(. ~ task, scales = "free", ncol = 3)

if (SAVEGG) {
  ggsave(gg_mboost,
    filename = FIG_DIR("fig-eq1.pdf"),
    width = DINA4WIDTH,
    height = DINA4WIDTH * 0.5,
    units = "mm")
}

# Get a table of mean runtimes:
df_mb %>%
  group_by(task, learner) %>%
  summarize(speedup = mean(speedup, na.rm = TRUE),
    time = mean(time, na.rm = TRUE)) %>%
  as.data.frame()
