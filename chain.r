# implement classifier chain
#
# Author: Xiru Lyu, Zhe Zhao
# Last Updated: Apr 11th, 2021


# set up: ---------------------------------------------------------------------
library(data.table)
library(rstanarm)
library(pROC)


# voting scheme
lcard_threshold = function(pred_list, cardinality){
  
  w = do.call(sum, pred_list) / length(pred_list)
  thresholds = seq(0.05, 0.95, by=0.05)
  best = which.min(abs(cardinality - sapply(thresholds, function(ts) 
    mean(rowSums(w >= ts)) 
    ) 
  ))
  t = thresholds[best]
  results = as.numeric(w >= t)
  
  return(results)
  
}


# Classifier Chain
cc = function(label,
              feature,
              chain_order,
              thresholds,
              alpha_nu = 5,
              beta_nu = 5,
              chains = 4,
              cores = 4,
              iter = 2000,
              warmup = floor(iter/2), 
              thin = 1){
  
  ccmodel = list(chain_order=chain_order) 
  ccmodel$models = list()
  
  for(i in chain_order){
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
    ccmodel$models[[i]] = fit_logit
    
    predicted = as.matrix(fit_logit, pars = "pred_prob")
    pred_lab = as.integer(colMeans(predicted) > thresholds[i])
    roc_obj = roc(label[[i]], pred_lab)
    print(auc(roc_obj))
    
    feature = cbind(feature, scaled_factor(pred_lab))
    colnames(feature)[length(colnames(feature))] = paste0('prev_', i)
  }
  
  class(ccmodel) = 'CC'
  
  return(ccmodel)
  
}

predict.CC = function(object, newdata, thresholds){
  
  predictions = list()
  for (lab in object$chain_order) {
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


# Emsemble Classifier Chain
ecc = function(label,
               feature,
               prior, 
               prior_intercept,
               chains, 
               cores, 
               thresholds,
               m = 10, 
               subsample = 0.75, 
               attr.space = 0.5){
  
  eccmodel = list(m=m)
  
  nrow = ceiling(nrow(feature) * subsample)
  ncol = ceiling(ncol(feature) * attr.space)
  eccmodel$cardinality = sum(label)/nrow(label)
  
  idx <- lapply(seq(m), function(iteration) {
    list(
      rows = sample(1:nrow(feature), nrow, replace = T),
      cols = sample(names(feature), ncol),
      chain_order = sample(names(label))
    )
  })
  
  eccmodel$models = parallel::mclapply(seq(m), function(iteration) {
    sub_feature = feature[idx[[iteration]]$rows, .SD, .SDcols = idx[[iteration]]$cols]
    print(sub_feature)
    chain_order = idx[[iteration]]$chain_order
    
    ccmodel = cc(label, sub_feature, chain_order, prior = prior, 
                  prior_intercept = prior_intercept, chains=chains, 
                 thresholds = thresholds, cores = 1)
    ccmodel$attrs = colnames(sub_feature)
    rm(sub_feature)
    
    ccmodel
    
  }, mc.cores = cores)
  
  class(eccmodel) <- "ECC"
  
  return(eccmodel)
  
}

predict.ECC = function(object, newdata, thresholds){
  
  all_preds = lapply(object$models, function(ccmodel) {
    predict.CC(ccmodel, newdata[, .SD, .SDcols = ccmodel$attrs], thresholds)
  })
  
  preds_list = lapply(all_preds, function(pred) {
    do.call(cbind, pred)
  }  ) 
  
  results = lcard_threshold(preds_list, object$cardinality)
  
  return(results)
}









