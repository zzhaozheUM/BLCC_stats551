# Run project 
#
# Author: Zhe Zhao
# Last Updated: Apr 14th, 2021

# set up: ---------------------------------------------------------------------
library(data.table)
library(rstanarm)
options(mc.cores = 12)

# set working directory to be file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# get functions
source('./chain.r')

# data: -----------------------------------------------------------------------
features = fread('./ATUS_covariates_new.csv')
labels = fread('./ATUS_sleep_periods.csv')

# truncate data to 2019 only
features = features[TUYEAR%in%c(2018,2019)]
labels = labels[TUCASEID %in% features$TUCASEID]
labels[, TUCASEID:=NULL]
features[, TUCASEID:=NULL]

# create factors
# standardize the data
f_varname = c("PEEDUCA", "PEMARITL", "PTDTRACE", "SEASON", "CHILD", 
              "STATE", "TESEX", "WEEKDAY", "TELFS2")
f_varname = paste(f_varname, collapse = "|")
X = cbind(labels$`04:00`, features)

fit_formula = V1 ~ TELFS2 + UNEMPLOYRATE + TEAGE + PEEDUCA + PEMARITL + PTDTRACE +
  TUYEAR + CHILD + TESEX + SEASON + STATE + WEEKDAY
X_fit = model.matrix(fit_formula, data = X)[,-1] # drop the intercept

# scale continuous variables
c_var = 0.5 * scale(X_fit[, -grep(f_varname, colnames(X_fit))])
# scale categorical variables
f_var = X_fit[, grep(f_varname, colnames(X_fit))]
f_var = apply(f_var, 2, function(x) {
  x = ifelse(x == 1, 1-sum(x)/length(x), -sum(x)/length(x))})

features = as.data.table(cbind(c_var, f_var))
names(features) = gsub(" ", "", names(features), fixed = TRUE)


# combine labels into 24 hours
stopifnot( length(labels)/60 == 24.0 )

idx = seq(1, 1440, 60)
hours = lapply(idx, function(i) labels[, i:(i+59) ])
hours = lapply(hours, function(col) rowSums(col))
hours = do.call(cbind, hours)
hours = as.data.table(hours>30)
hours = hours*1

# test ECC
cc_model = cc(hours, features, chain_order = names(hours), 
              prior = student_t(5, 0, 2.5), 
              prior_intercept = student_t(5, 0, 2.5), 
              chains=2, cores=1)

ecc_model = ecc(hours, features, prior = student_t(7, 0, 2.5), 
    prior_intercept = student_t(7, 0, 2.5), chains = 1, cores=2, m = 10, 
    subsample = 0.75, attr.space = 0.5)

