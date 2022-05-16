getTraceData = function(file, end_dataio, end_initphase, end_fittingphase) {
  dat = read.csv(file, header = FALSE)

  phase = c(rep("read data", end_dataio), rep("init model", end_initphase - end_dataio))
  if (! is.na(end_fittingphase)) {
    phase = c(phase, rep("fitting", end_fittingphase - end_initphase))
  }

  dat$kb_free = as.integer(gsub("kB", "", dat$V2))
  dat$mb_used = (dat$kb_free[1] - dat$kb_free) / 1024

  ilast = ifelse(is.na(end_fittingphase), end_initphase, end_fittingphase)
  dat = dat[seq_len(ilast), ]
  dat$date = as.POSIXct(dat$V1, format = "%Y-%m-%d-%H:%M:%S")
  dat$seconds = dat$date - dat$date[1]
  dat$seconds = seq(dat$seconds[1], dat$seconds[ilast], length.out = ilast)

  out = data.frame(second = dat$seconds, mb_used = dat$mb_used, phase = phase)
  return(out)
}


measureMem = function(file = NA, sleep = NA, rm = TRUE) {
  if (is.na(file)) {
    cmd = ""
  } else {
    if (file.exists(file)) file.remove(file)
    cmd = paste0(" >> ", file)
  }
  run = TRUE
  while(run) {
    system(paste0("cat /proc/meminfo | grep MemAvailable | awk -v date=\"$(date +%Y-%m-%d-%H:%M:%S)\" 'FNR == 1 {print date \",\" $2 $3}'", cmd))

    if (! is.na(file))
      print(tail(read.csv(file, header = FALSE), n = 1))

    if (is.na(sleep)) {
      run = FALSE
      sleep = 0
    }
    Sys.sleep(sleep)
  }
}

last = function(x) tail(x, 1)


measureMem()

library(ggplot2)

ram_limit = 31617924 / 1024

sleep = 0.2

## HIGGS data:
## =======================================================

#measureMem("mem-higgs-nobinning.txt", sleep)
dhn = getTraceData("mem-higgs-nobinning.txt", 64, 1184, NA)
dhn = dhn[seq_len(which.max(dhn$mb_used)), ]
offset = ram_limit - max(dhn$mb_used)
dhn$mb_used = offset + dhn$mb_used
dhn = rbind(data.frame(second = 0, mb_used = 0, phase = "idle"), dhn)

dhn$data = "Higgs"
dhn$bin = "No binning"


#measureMem("mem-higgs-binning.txt", sleep)
dhb = getTraceData("mem-higgs-binning.txt", 58, 821, 1674)
dhb$mb_used = offset + dhb$mb_used
dhb = rbind(data.frame(second = 0, mb_used = 0, phase = "idle"), dhb)
dhb$data = "Higgs"
dhb$bin = "Binning"

dh = rbind(dhn, dhb)

dhn$mb_used = dhn$mb_used / 1024
dhb$mb_used = dhb$mb_used / 1024
dh$mb_used = dh$mb_used / 1024
ram_limit_gb = ram_limit / 1024

xoff = diff(range(dh$second)) * 0.035
d_accent_h = rbind(
  data.frame(x = last(dhn$second), xoff = last(dhn$second) + xoff, y = last(dhn$mb_used),
    data = "Higgs", bin = "No binning", label = "Initialization\ncrashes"),
  data.frame(x = dhb$second[59], xoff = dhb$second[59] + xoff, y = dhb$mb_used[59],
    data = "Higgs", bin = "Binning", label = "Start initialization"),
  data.frame(x = dhb$second[822], xoff = dhb$second[822] + xoff, y = dhb$mb_used[822],
    data = "Higgs", bin = "Binning", label = "Start fitting"))


## New York taxi data:
## =======================================================

#measureMem("mem-nyt-nobinning.txt", sleep)
dnyn = getTraceData("mem-nyt-nobinning.txt", 108, 859, NA)
dnyn = dnyn[seq_len(which.max(dnyn$mb_used)), ]
offset = ram_limit - max(dnyn$mb_used)
dnyn$mb_used = offset + dnyn$mb_used
dnyn = rbind(data.frame(second = 0, mb_used = 0, phase = "idle"), dnyn)

dnyn$data = "New York Taxi"
dnyn$bin = "No binning"


#measureMem("mem-nyt-binning.txt", sleep)
dnyb = getTraceData("mem-nyt-binning.txt", 108, 2260, 3898)
dnyb$mb_used = offset + dnyb$mb_used
dnyb = rbind(data.frame(second = 0, mb_used = 0, phase = "idle"), dnyb)
dnyb$data = "New York Taxi"
dnyb$bin = "Binning"

dny = rbind(dnyn, dnyb)

dnyn$mb_used = dnyn$mb_used / 1024
dnyb$mb_used = dnyb$mb_used / 1024
dny$mb_used = dny$mb_used / 1024

xoff = diff(range(dny$second)) * 0.035
d_accent_ny = rbind(
  data.frame(x = last(dnyn$second), xoff = last(dnyn$second) + xoff, y = last(dnyn$mb_used),
    data = "New York Taxi", bin = "No binning", label = "Initialization crashes"),
  data.frame(x = dnyb$second[109], xoff = dnyb$second[109] + xoff, y = dnyb$mb_used[109],
    data = "New York Taxi", bin = "Binning", label = "Start initialization"),
  data.frame(x = dnyb$second[2287], xoff = dnyb$second[2287] + xoff, y = dnyb$mb_used[2287],
    data = "New York Taxi", bin = "Binning", label = "Start fitting"))


## Flood Insurance:
## =======================================================

#measureMem("mem-flood-nobinning.txt", sleep)
dfn = getTraceData("mem-flood-nobinning.txt", 290, 1175, NA)
dfn = dfn[seq_len(which.max(dfn$mb_used)), ]
offset = ram_limit - max(dfn$mb_used)
dfn$mb_used = offset + dfn$mb_used
dfn = rbind(data.frame(second = 0, mb_used = 0, phase = "idle"), dfn)

dfn$data = "Flood Insurance"
dfn$bin = "No binning"


#measureMem("mem-flood-binning.txt", sleep)
dfb = getTraceData("mem-flood-binning.txt", 235, 1830, 4041)
dfb$mb_used = offset + dfb$mb_used
dfb = rbind(data.frame(second = 0, mb_used = 0, phase = "idle"), dfb)
dfb$data = "Flood Insurance"
dfb$bin = "Binning"

df = rbind(dfn, dfb)

dfn$mb_used = dfn$mb_used / 1024
dfb$mb_used = dfb$mb_used / 1024
df$mb_used = df$mb_used / 1024

xoff = diff(range(df$second)) * 0.035
d_accent_f = rbind(
  data.frame(x = last(dfn$second), xoff = last(dfn$second) + xoff, y = last(dfn$mb_used),
    data = "Flood Insurance", bin = "No binning", label = "Initialization crashes"),
  data.frame(x = dfb$second[236], xoff = dfb$second[236] + xoff, y = dfb$mb_used[236],
    data = "Flood Insurance", bin = "Binning", label = "Start initialization"),
  data.frame(x = dfb$second[1831], xoff = dfb$second[1831] + xoff, y = dfb$mb_used[1831],
    data = "Flood Insurance", bin = "Binning", label = "Start fitting"))


## VISUALIZATION:
## ======================================================

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

# important dirs:
#BASE_DIR = here::here("appendix/")
BASE_DIR = "~/temp/cacb-revision/feasibility/"
FIG_DIR  = function(fig_name) paste0(BASE_DIR, "/figures/", fig_name)


SAVEGG = TRUE

dplot = rbind(dh, dny, df)
dacc = rbind(d_accent_h, d_accent_ny, d_accent_f)
dats = c("Higgs", "New York Taxi", "Flood Insurance")
dhline = data.frame(
  yint = c(dh$mb_used[2], dny$mb_used[2], df$mb_used[2]),
  data = dats)

dhline$data = factor(dhline$data, levels = dats)
dplot$data = factor(dplot$data, levels = dats)
dacc$data = factor(dacc$data, levels = dats)


gg = ggplot() +
  geom_hline(yintercept = ram_limit_gb, color = "dark red", linetype = "dashed") +
  geom_text(data = data.frame(x = 0, y = ram_limit_gb + 0.5, label = "RAM limit", data = "Higgs"),
    aes(x = x, y = y, label = label), color = "dark red", hjust = 0, vjust = 0, size = 3) +
  facet_grid(. ~ factor(data, levels = dats), scales = "free_x") +
  geom_hline(data = dhline, aes(yintercept = yint), color = "dark blue") +
  geom_text(data = dhline[1, ], aes(x = 0, y = yint, label = "Memory used by background processes"),
    color = "dark blue", hjust = 0, vjust = 1, size = 3) +

  geom_line(data = dplot, aes(x = second, y = mb_used, color = bin)) +

  geom_point(data = dacc, aes(x = x, y = y), size = 4) +
  geom_label(data = dacc, aes(x = xoff, y = y, label = label, fill = bin),
    color = "white", fontface = "bold", hjust = 0, vjust = 0.5, size = 3) +

    #annotate("text", x = 100, y = dh$mb_used[2] - 0.5, label = "Memory used by background processes",
    #color = "dark blue") +
  labs(color = "", fill = "") +
  xlab("Seconds") +
  ylab("RAM (in GB)") +
  ggsci::scale_color_uchicago() +
  ggsci::scale_fill_uchicago() +
  theme(legend.position = "bottom")

if (SAVEGG) {
  ggsave(gg,
    filename = FIG_DIR("app-big-data.pdf"),
    width = DINA4WIDTH * 1.5,
    height = DINA4WIDTH * 0.75,
    units = "mm")
}


