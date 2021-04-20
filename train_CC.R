# Run project 
#
# Author: Xiru Lyu, Zhe Zhao
# Last Updated: Apr 14th, 2021

# set up: ---------------------------------------------------------------------
library(data.table)
library(rstanarm)
options(mc.cores = 12)

# set working directory to be file directory
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# get functions
source('./chain.r')
source("Data_scaling.R")

# data: -----------------------------------------------------------------------
X = data.table::fread("ATUS_covariates_new.csv")
labels = data.table::fread("ATUS_sleep24.csv")
X_mat = scaled_data(X)

# stan model: -----------------------------------------------------------------
mod_c = stanc("logit.stan")
model = stan_model(stanc_ret = mod_c)

# test ECC: -------------------------------------------------------------------
thresholds = floor(colMeans(labels)*100)/100
cc_model = cc(label=labels, feature = X_mat, chain_order = colnames(labels),
              thresholds = thresholds)

ecc_model = ecc(hours, features, prior = student_t(7, 0, 2.5), 
    prior_intercept = student_t(7, 0, 2.5), chains = 1, cores=2, m = 10, 
    subsample = 0.75, attr.space = 0.5)

