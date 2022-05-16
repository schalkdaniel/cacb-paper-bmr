ddir = "~/temp/cacb-revision/feasibility/new-york-taxi/"

# Download from: https://www.kaggle.com/datasets/elemento/nyc-yellow-taxi-trip-data
dnames = paste0(ddir, c("yellow_tripdata_2016-01.csv",
  "yellow_tripdata_2016-02.csv", "yellow_tripdata_2016-03.csv"))

library(mlr3)
library(mlr3pipelines)

robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")

for (dn in dnames) {

  cat("Read", dn, "\n")
  dat = data.table::fread(dn)

  dat$passenger_count = as.factor(dat$passenger_count)
  dat$RatecodeID = as.factor(dat$RatecodeID)
  dat$payment_type = as.factor(dat$payment_type)
  dat$extra = as.factor(dat$extra)
  dat$mta_tax = as.factor(dat$mta_tax)


  dat$VendorID = NULL

  dat$pickup_day_of_month = as.integer(format(dat$tpep_pickup_datetime, "%d"))
  dat$pickup_hour = as.integer(format(dat$tpep_pickup_datetime, "%H"))
  dat$pickup_minute = as.integer(format(dat$tpep_pickup_datetime, "%M"))

  dat$dropoff_day_of_month = as.integer(format(dat$tpep_dropoff_datetime, "%d"))
  dat$dropoff_hour = as.integer(format(dat$tpep_dropoff_datetime, "%H"))
  dat$dropoff_minute = as.integer(format(dat$tpep_dropoff_datetime, "%M"))

  dat$ride_time = as.numeric(dat$tpep_dropoff_datetime - dat$tpep_pickup_datetime)

  dat$tpep_dropoff_datetime = NULL
  dat$tpep_pickup_datetime = NULL

  cat("Process", dn, "\n")
  task = as_task_regr(dat, target = "total_amount")
  dat = robustify$train(task)[[1]]$data()

  saveRDS(dat, gsub(".csv", ".Rda", dn))
  cat("Successfully saved", dn, "\n")
}

dat = do.call(rbind, lapply(dnames, function(dn) readRDS(gsub(".csv", ".Rda", dn))))
saveRDS(dat, paste0(ddir, "data-merged.Rda"))
