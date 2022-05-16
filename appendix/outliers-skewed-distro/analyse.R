simData = function(data, job, nsim, sds, shape1, shape2, pout, ...) {
  mySpline = function(x) {
    nk = 8
    xu = 1
    xl = 0

    xr = xu - xl
    xl = xl - xr * 0.001
    xu = xu + xr * 0.001

    dx = (xu - xl)/(nk - 1)
    kn = seq(xl - dx * 3, xu + dx * 3, length = nk + 4 + 2)

    # create the spline basis functions
    X = splines::spline.des(kn, x, 4, x * 0)$design

    # multiply with random coefficients to get random functions
    coefs = c(3.3910, -1.1331, -1.6428, -0.8845, 0.7561, 0.5241, 1.0630, -1.6349, 0.3582, -3.8201)

    return(X %*% coefs)
  }


  x = rbeta(nsim, shape1 = shape1, shape2 = shape2)
  y = mySpline(x) + rnorm(nsim, sd = 0.2)
  if (sds > 0) {
    noutlier = ceiling(pout * nsim)
    sdx = sd(x)
    idx = sample(seq_along(x), noutlier)
    x[idx] = sdx * sds + max(x) + rnorm(noutlier, sd = sdx)
  }
  list(data = data.frame(x, y), spFun = mySpline)
}



#instance = simData(nsim = 10000, sds = 2, shape1 = 2, shape2 = 4)
#plot(instance$data)
#curve(instance$spFun(x), from = 0, to = 1, col = "red", add = TRUE)

cwbAlgo = function(data, job, instance, bin_root, ...) {
  skewness = function(x) mean((x - mean(x))^3) / var(x)^(3/2)
  ise = function(x, y1, y2) {
    se = (y1 - y2)^2
    f = approxfun(x = x, y = se)
    return(integrate(f, lower = 0, upper = 1)$value)
  }


  dat = instance$data
  cwb = boostSplines(dat, "y", loss = LossQuadratic$new(), iterations = 200L, df = 5L, bin_root = bin_root)

  sk = skewness(dat$x)

  newdata = data.frame(x = seq(0, 1, length.out = 1000L))

  pred = cwb$predict(newdata)
  truth = instance$spFun(newdata$x)

  is = ise(newdata$x, pred, truth)

  pp = job$pars$prob.pars
  ap = job$pars$algo.pars

  return(data.frame(job = job$id, repl = job$repl, algo = job$algo.name, n = pp$nsim,
    sds = pp$sds, shape1 = pp$shape1, shape2 = pp$shape2, pout = pp$pout,
    bin_root = ap$bin_root, skewness = sk, ise = is))
}



## BATCHTOOLS:
## ===================================================================

library(batchtools)

base_dir = "~/temp/cacb-revision"
bt_dir = paste0(base_dir, "/batchtools")

# unlink(bt_dir, TRUE)

reg = makeExperimentRegistry(
  file.dir = bt_dir,
  packages = "compboost",
  seed     = 31415)

reg$cluster.functions = makeClusterFunctionsMulticore(ncpus = parallel::detectCores() - 2L)

saveRegistry(reg)

addProblem("simdat", fun = simData, seed = 31415L)
addAlgorithm("cwb", fun = cwbAlgo)

# algorithm design:
ades = list(cwb = data.frame(bin_root = c(1, 2)))

# problem design:
nsim = c(500, 1000, 5000, 10000)
pdes = list(simdat = rbind(
  expand.grid(
    nsim = nsim,
    sds  = c(4, 16, 64),
    shape1 = 2,
    shape2 = 2,
    pout   = c(0.01, 0.05, 0.1)
  ),
  expand.grid(
    nsim = nsim,
    sds  = 0,
    shape1 = c(1, 3, 5, 7, 9),
    shape2 = c(1, 3, 5, 7, 9),
    pout   = 0
  )
))
#pdes = list(simdat = expand.grid(
  #nsim   = c(1000, 5000, 25000),
  #sds    = c(0, 4, 16),
  #shape1 = c(1, 3, 6, 9, 10),
  #shape2 = c(1, 3, 6, 9, 10),
  #pout   = c(0.01, 0.05, 0.1)))


addExperiments(prob.design = pdes, algo.design = ades, repls = 50L)

submitJobs()

## VISUALIZE:
## =============================================

library(dplyr)
library(ggplot2)
library(batchtools)

## FIGURE SETTINGS:
## ---------------------------------------------

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
#BASE_DIR = here::here("appendix/")
BASE_DIR = "~/temp/cacb-revision/stress/"
BT_DIR   = paste0(BASE_DIR, "batchtools/")
FIG_DIR  = function(fig_name) paste0(BASE_DIR, "/figures/", fig_name)

## LOAD DATA:
## ============================================

loadRegistry(BT_DIR)
ll = reduceResultsList(findDone())
dres = do.call(rbind, ll)


# Data for skewed distribution:
dskew = dres %>% filter(shape1 != 2)

# Data for outlier:
dout = dres %>% filter(shape1 == 2)


## SKEWED FEATURE DISTRIBUTION:
## ============================================

dskewdiff = dskew %>%
  group_by(repl, algo, n, shape1, shape2) %>%
  summarize(ise_diff = (ise[bin_root == 1] - ise[bin_root == 2]) / ise[bin_root == 1], skewness = skewness[1]) %>%
  #group_by(algo, n, shape1, shape2) %>%
  #summarize(skewness = mean(skewness), mise_diff = mean(ise_diff)) %>%
  mutate(skewness_cat = cut(skewness, breaks = c(-Inf, -2, -1, -0.5, -0.1, 0.1,  0.5, 1, 2, Inf)))

gg_skew = dskewdiff %>%
  ggplot(aes(x = skewness_cat, y = ise_diff, fill = skewness_cat)) +
    geom_boxplot(alpha = 0.5) +
    ggsci::scale_fill_uchicago() +
    ggsci::scale_color_uchicago() +
    ylab("Relative change of MISE") +
    xlab("Skewness") +
    facet_grid(. ~ n, labeller = labeller(n = function(x) paste0("n: ", x))) +
    labs(fill = "Skewness", color = "Skewness") +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    theme(legend.position = "none",
      #axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
      axis.ticks.x = element_blank())

if (SAVEGG) {
  ggsave(gg_skew,
    filename = FIG_DIR("app-skewness-mise-diff.pdf"),
    width = DINA4WIDTH,
    height = DINA4WIDTH * 0.5,
    units = "mm")
}



# Skewness 1.5
pars = list(c(1, 9), c(3, 9), c(5, 5), c(9, 3))
ggs = list()
for (k in seq_along(pars)) {
  pp = pars[[k]]
  sks = numeric(50L)
  for (i in seq_len(50L)) {
    x = rbeta(100000, shape1 = pp[1], shape2 = pp[2])
    sks[i] = e1071::skewness(x)
  }
  ggs[[k]] = ggplot() +
    geom_function(fun = dbeta, args = list(shape1 = pp[1], shape2 = pp[2])) +
    ggtitle(paste0("Skewness: ", round(mean(sks), 2))) +
    xlab("x") +
    ylab(paste0("Beta(", pp[1], ",", pp[2], ") Density")) +
    theme(plot.title = element_text(size = 10)) +
    scale_x_continuous(breaks = c(0, 0.5, 1))
}
gg_distros = do.call(gridExtra::grid.arrange, c(ggs, list(ncol = 4)))

if (SAVEGG) {
  ggsave(gg_distros,
    filename = FIG_DIR("app-distr.pdf"),
    width = DINA4WIDTH,
    height = DINA4WIDTH * 0.3,
    units = "mm")
}


## OUTLIER:
## ============================================

doutdiff = dout %>%
  group_by(repl, algo, n, sds, pout) %>%
  summarize(ise_diff = (ise[bin_root == 1] - ise[bin_root == 2]) / ise[bin_root == 1], count = n()) %>%
  mutate(sds = as.factor(sds))

gg_out = doutdiff %>%
  ggplot(aes(x = sds, y = ise_diff, fill = sds)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "dark red") +
  geom_boxplot(alpha = 0.5) +
  ggsci::scale_fill_uchicago() +
  ggsci::scale_color_uchicago() +
  ylab("Relative change of MISE") +
  xlab("Outlier distance as multiplicative of sd") +
  labs(fill = "Multiplicative of sd") +
  guides(fill = "none", color = "none") +
  theme(legend.position = "bottom") +
  facet_grid(pout ~ n, scale = "free", labeller = labeller(
    pout = function(x) paste0("q: ", x),
    n = function(x) paste0("n: ", x)))

if (SAVEGG) {
  ggsave(gg_out,
    filename = FIG_DIR("app-outlier-mise-diff.pdf"),
    width = DINA4WIDTH * 0.7,
    height = DINA4WIDTH * 0.5,

    units = "mm")
}

