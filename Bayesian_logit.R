# load packages
library(rstan)
library(tidyverse)

# get functions
source("Data_scaling.R")
source("logit_func.R")

# load data
X = data.table::fread("ATUS_covariates2018.csv")
labels = data.table::fread("ATUS_labels2018.csv")
X_pred = data.table::fread("ATUS_covariates2019.csv")
labels_pred = data.table::fread("ATUS_labels2019.csv")
X_mat = scaled_data(X)

# stan model
mod_c = stanc("logit.stan")
model = stan_model(stanc_ret = mod_c)

# obtain the model
bayesmodel = bayes_logit(labels, X_mat, cores = 4)

# obatain the predicted labels
thresholds = apply(labels, 2, mean)
prediction = predict.bayes_logit(bayesmodel, X_pred, thresholds = thresholds)

