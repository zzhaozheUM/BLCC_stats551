# Run project 
#
# Author: Zhe Zhao
# Last Updated: Apr 14th, 2021

# set up: ---------------------------------------------------------------------
library(data.table)

# get functions
source('chain.r')
source("Data_scaling.R")
source("metrics.R")

# data: -----------------------------------------------------------------------
X = data.table::fread("ATUS_covariates2018.csv")
labels = data.table::fread("ATUS_labels2018.csv")
X_pred = data.table::fread("ATUS_covariates2019.csv")
labels_pred = data.table::fread("ATUS_labels2019.csv")
X_mat = scaled_data(X)

# stan model: -----------------------------------------------------------------
mod_c = stanc("logit.stan")
model = stan_model(stanc_ret = mod_c)

thresholds = floor(colMeans(labels)*100)/100
cc_model = cc(labels = labels, feature = X_mat, chain_order = colnames(labels),
              thresholds = thresholds, chains = 4, cores = 4)
ecc_model = ecc(label = labels, feature = X_mat, thresholds = thresholds,
                chains = 4, cores = 8)

# prediction
cc_pred = predict.CC(cc_model, X_pred, thresholds = thresholds,
                     orders = colnames(labels), cores = 8)
ecc_pred = predict.ECC(ecc_model, X_pred, thresholds =  thresholds, 
                         orders = colnames(labels), cores = 8)

# accuracy rate
cc_accuracy = multi_accuracy(cc_pred, labels_pred)
ecc_accuracy = multi_accuracy(ecc_pred, labels_pred)

# macro F1
cc_macrof1 = macro_F1(cc_pred, labels_pred)
ecc_macrof1 = macro_F1(ecc_pred, labels_pred)

# micro F1
cc_microf1 = micro_F1(cc_pred, labels_pred)
ecc_microf1 = micro_F1(ecc_pred, labels_pred)