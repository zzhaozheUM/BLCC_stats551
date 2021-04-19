# load packages
library(rstan)
library(tidyverse)

bayes_logit = function(label,
                       feature,
                       alpha_nu = 5,
                       beta_nu = 5,
                       chains = 4,
                       cores = 4,
                       iter = 2000,
                       warmup = floor(iter/2), 
                       thin = 1) {
  bayesmodel = list()
  bayesmodel$models = list()
  for (i in colnames(label)) {
    y = label %>% select(i) %>% unlist(use.names = FALSE)
    data = list(N = nrow(feature),
                p = dim(feature)[2],
                alpha_nu = alpha_nu,
                alpha_m = 0,
                alpha_s = 10,
                beta_nu = rep(beta_nu, dim(feature)[2]),
                beta_m = rep(0, dim(feature)[2]),
                beta_s = rep(2.5, dim(feature)[2]),
                X = feature,
                y = y)
    fit_logit = sampling(model, data = data, chains = chains, cores = cores,
                         iter = iter, warmup = warmup, thin = thin)
    bayesmodel$models[[i]] = fit_logit
  }
  
  class(bayesmodel) = 'CC'
  
  return(bayesmodel)
}

predict.bayes_logit = function(object, newdata, thresholds) {
  predictions = list()
  for (lab in object$models) {
    model = object$models[[lab]]
    ext_fit = rstan::extract(model)
    alpha_post = matrix(ext_fit$alpha, length(ext_fit$alpha), nrow(newdata))
    beta_post = ext_fit$beta
    lp = alpha_post + beta_post %*% t(newdata)
    probs = 1/(1 + exp(-lp))
    pred_lab = as.integer(colMeans(probs) > thresholds[[lab]])
    newdata[[paste0('prev_', lab)]] = pred_lab
    
    predictions[[lab]] = pred_lab
  }
  
  return(predictions)
}