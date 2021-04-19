# load packages
library(rstan)
library(tidyverse)

# get functions
source("../Data_scaling.R")
source("./logit_func.R")

# load data
X = data.table::fread("../ATUS_covariates2018.csv")
labels = data.table::fread("./ATUS_labels2018.csv")
X_pred = data.table::fread("./ATUS_covariates2019.csv")
labels_pred = data.table::fread("./ATUS_labels2019.csv")
X_mat = scaled_data(X)

# stan model
mod_c = stanc("logit.stan")
model = stan_model(stanc_ret = mod_c)

# obtain the model
bayesmodel = bayes_logit(labels, X_mat, cores = 4)

thresholds = apply(labels, 2, mean)
X_pred_mat = scaled_data(X_pred)
predictions = list()
for (lab in names(labels_pred)) {
  model = load(sprintf('./BLR/logit_%s.RData', lab))
  ext_fit = rstan::extract(fit_logit)
  alpha_post = matrix(ext_fit$alpha, length(ext_fit$alpha), nrow(X_pred_mat))
  beta_post = ext_fit$beta
  lp = alpha_post + beta_post %*% t(X_pred_mat)
  probs = 1/(1 + exp(-lp))
  pred_lab = as.integer(colMeans(probs) > thresholds[[lab]])
  
  predictions[[lab]] = pred_lab
}

# obtain the predicted labels

source('./metrics.R')
load('../BLR/predictions_BLR.RData')
pred_dt = do.call(cbind, predictions)
multi_accuracy(labels_pred, pred_dt) #0.4412752
macro_F1(labels_pred, pred_dt) # 0.3859952
micro_F1(labels19, pred_dt) # 0.5470214



