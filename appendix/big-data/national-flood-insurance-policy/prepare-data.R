dat = data.table::fread("~/temp/cacb-revision/feasibility/national-flood-insurance-policy/nfip-flood-policies.csv")
dat = dat[dat$originalconstructiondate > as.Date("1990-01-01"), ]

date_cols = c("policyeffectivedate", "policyterminationdate", "originalconstructiondate", "originalnbdate", "cancellationdateoffloodpolicy")
factor_cols = c("basementenclosurecrawlspacetype", "condominiumindicator", "construction", "deductibleamountinbuildingcoverage", "elevatedbuildingindicator",
  "deductibleamountincontentscoverage", "elevationcertificateindicator", "floodzone",
  "nonprofitindicator", "numberoffloorsininsuredbuilding", "obstructiontype", "occupancytype", "policycount",
  "policytermindicator", "postfirmconstructionindicator", "primaryresidenceindicator", "propertystate", "ratemethod",
  "regularemergencyprogramindicator", "reportedcity")

dat$hfiaasurcharge = NULL
dat$houseofworshipindicator = NULL
dat$smallbusinessindicatorbuilding = NULL

for (dc in date_cols) {
  dat[[paste0(dc, "_day_of_month")]] = as.integer(format(dat[[dc]], "%d"))
  dat[[paste0(dc, "_month")]] = as.integer(format(dat[[dc]], "%m"))
  dat[[paste0(dc, "_year")]] = as.integer(format(dat[[dc]], "%Y"))

  dat[[dc]] = NULL
}

for (fc in factor_cols) {
  dat[[fc]] = as.factor(dat[[fc]])
}

dat$elevationdifference[dat$elevationdifference == 999] = NA

for (i in seq_along(dat)) {
  if (inherits(dat[[i]], "integer64"))
    dat[[i]] = as.numeric(dat[[i]])
}

saveRDS(dat, "~/temp/cacb-revision/feasibility/national-flood-insurance-policy/intermediate1.Rda")
dat = readRDS("~/temp/cacb-revision/feasibility/national-flood-insurance-policy/intermediate1.Rda")



library(mlr3)
library(mlr3pipelines)

robustify = po("removeconstants", id = "removeconstants_before") %>>%
    po("imputemedian", id = "imputemedian_num", affect_columns = selector_type(c("integer", "numeric"))) %>>%
    po("imputemode", id = "imputemode_fct", affect_columns = selector_type(c("character", "factor", "ordered"))) %>>%
    po("collapsefactors", target_level_count = 10) %>>%
    po("removeconstants", id = "removeconstants_after")

task = as_task_regr(dat, target = "totalinsurancepremiumofthepolicy")
d0 = robustify$train(task)[[1]]$data()

# rm cols with too many missings:
#msgs = sapply(d0, function(x) mean(is.na(x)))
dat$obstructiontype = NULL

#table(sapply(d0, class))
#character    factor   integer   numeric
        #3        19        22         7

#dim(d0)
#[1] 16486845       51

#object.size(d0) / 1024^3
#3.7 bytes

saveRDS(d0, "~/temp/cacb-revision/feasibility/national-flood-insurance-policy/nfip-flood-policies.Rda")

if (FALSE) {
  library(compboost)

  mod = boostSplines(data = d0, target = task$target_names, loss = LossQuadratic$new())
}

