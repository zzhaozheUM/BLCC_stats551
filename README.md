# Bayesian Logistic Classifier Chains

## data
ATUS_covariates2018.csv: features for training set
ATUS_labels2018.csv: labels for training set
ATUS_covariates2019.csv: features for test set
ATUS_labels2019.csv: labels for test set

## Model
Chain.R: Classifier Chains training and prediction algorithm
prepare_BLR.R: Bayesian logistic regression training and predition alrorithm for independent labels
logit.stan: stan file to specify Bayesian Logistic Regression

## training and prediction
train_BLR.R: training and predicting BLR model
train_CC.R: training and predicting classifier chains model
metrics.R: defines the evaluation metrics

## Contribution:
Xiru and Zhe both contributed greatly to this project. Xiru worked on setting up Bayesian logistic regression models and implement them into CC. Zhe worked implemented CC algorithm and its prediction. They both conduct training and evaluations.
