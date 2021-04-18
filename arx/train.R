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
labels = fread('./ATUS_sleep24.csv')

# create factors
# standardize the data
f_varname = c("PEEDUCA", "PEMARITL", "PTDTRACE", "SEASON", "CHILD", 
              "GEREG", "TESEX", "WEEKDAY", "TELFS2")

for(x in f_varname){
  features[[x]] = as.factor(features[[x]])
}

#f_varname = paste(f_varname, collapse = "|")
#X = cbind(labels$`04:00`, features)

#fit_formula = V1 ~ TELFS2 + UNEMPLOYRATE + TEAGE + PEEDUCA + PEMARITL + PTDTRACE +
#  CHILD + TESEX + SEASON + region + WEEKDAY
#X_fit = model.matrix(fit_formula, data = X)[,-1] # drop the intercept

# scale continuous variables
#c_var = 0.5 * scale(X_fit[, -grep(f_varname, colnames(X_fit))])
# scale categorical variables
#f_var = X_fit[, grep(f_varname, colnames(X_fit))]
#f_var = apply(f_var, 2, function(x) {
#  x = ifelse(x == 1, 1-sum(x)/length(x), -sum(x)/length(x))})

#features = as.data.table(cbind(c_var, f_var))
#names(features) = gsub(" ", "", names(features), fixed = TRUE)

#rm(f_var, X_fit)


# test ECC: -------------------------------------------------------------------
thresholds = floor(colMeans(labels)*100)/100
cc_model = cc(labels, features, chain_order = names(labels), 
              prior = student_t(5, 0, 2.5, autoscale = T), 
              prior_intercept = student_t(5, 0, 2.5, autoscale = T),
              chains=4, thresholds = thresholds, cores=1)

ccpredictions = predict.CC(cc_model, features, thresholds)

save(cc_model, file = './cc_model.RData')

for (m in cc_model$models) {
  bayesplot::color_scheme_set("mix-teal-pink")
  trace = plot(m, "trace", pars = "(Intercept)")
  print(trace)
}

ecc_model = ecc(labels, features, prior = student_t(5, 0, 2.5, autoscale = T), 
    prior_intercept = student_t(5, 0, 2.5, autoscale = T), chains = 4, cores=10, thresholds,
    m = 10, subsample = 0.75, attr.space = 0.5)
save(ecc_model, file = './ecc_model.RData')

predict.ECC(ecc_model, features, thresholds)
