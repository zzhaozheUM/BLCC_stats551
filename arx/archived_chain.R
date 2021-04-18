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
  
  w = Reduce("+" , pred_list) / length(pred_list)
  thresholds = seq(0.05, 0.95, by=0.05)
  best = which.min(abs(cardinality - sapply(thresholds, function(ts) 
    mean(rowSums(w >= ts)) 
  ) 
  ))
  t = thresholds[best]
  results = 1*(w >= t)
  
  return(results)
  
}


# Classifier Chain
cc = function(label,
              feature,
              chain_order,
              prior, 
              prior_intercept,
              chains,
              thresholds,
              cores){
  
  ccmodel = list(chain_order=chain_order) 
  num_label = ncol(label)
  var_feat = names(feature)
  dat = cbind(label, feature)
  
  ccmodel$models = list()
  for(i in chain_order){
    var_lab = i
    rhs = paste(var_feat, collapse ='+')
    formula = as.formula(paste(var_lab, '~', rhs, sep = ''))
    classifier = stan_glm(formula=formula, data = dat, family = binomial(link='logit'),
                          prior = prior, prior_intercept = prior_intercept, chains = chains, 
                          cores = cores, QR = T)
    
    ccmodel$models[[i]] = classifier
    
    probs = posterior_epred(classifier)
    pred_lab = as.integer(colMeans(probs) > thresholds[var_lab])
    roc_obj = roc(label[[var_lab]], pred_lab)
    print(auc(roc_obj))
    #pred_lab = ifelse(pred_lab == 1, 1-sum(pred_lab)/length(pred_lab), 
    #                  -sum(pred_lab)/length(pred_lab))
    dat[[paste0('prev_', i)]] = pred_lab
    var_feat = c(var_feat, paste0('prev_', i))
    
  }
  
  rm(dat)
  
  class(ccmodel) = 'CC'
  
  return(ccmodel)
  
}

predict.CC = function(object, newdata, thresholds){
  
  predictions = list()
  for (lab in object$chain_order) {
    model = object$models[[lab]]
    probs = posterior_epred(model, newdata = newdata)
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
    
    label = label[idx[[iteration]]$rows, ]
    
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

predict.ECC = function(object, newdata, thresholds, orders){
  
  all_preds = lapply(object$models, function(ccmodel) {
    predict.CC(ccmodel, newdata[, .SD, .SDcols = ccmodel$attrs], thresholds)
  })
  
  preds_list = lapply(all_preds, function(pred) {
    do.call(cbind, pred)
  }  ) 
  
  preds_list = lapply(preds_list, function(pred) {
    pred[, orders]
  }  ) 
  
  results = lcard_threshold(preds_list, object$cardinality)
  
  return(results)
}









