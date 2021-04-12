# implement classifier chain
#
# Author: Xiru Lyu, Zhe Zhao
# Last Updated: Apr 11th, 2021


# set up: ---------------------------------------------------------------------
library(data.table)
library(rstanarm)
options(mc.cores = 6)


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
              prior, 
              prior_intercept,
              chains,
              cores){
  
  ccmodel = list(chain_order=chain_order) 
  num_label = ncol(label)
  var_feat = names(feature)
  dat = cbind(label, feature)
  
  ccmodel$models = list()
  for(i in chain_order){
    var_lab = names(label)[i]
    rhs = paste(var_feat, collapse ='+')
    formula = as.formula(paste(var_lab, '~', rhs, sep = ''))
    classifier = stan_glm(formula=formula,
                          data = dat,
                          family = binomial(link='logit'),
                          prior = prior,
                          prior_intercept = prior_intercept,
                          chains = chains,
                          cores = cores
    )
    
    ccmodel$models[[i]] = classifier
    
    probs = posterior_linpred(classifier, transform = TRUE)
    pred_lab = as.integer(colMeans(preds) > 0.5)
    dat[[paste0('prev_', i)]] = pred_lab
    var_feat = c(var_feat, paste0('prev_', i))
     
  }
  
  rm(dat)
  
  class(ccmodel) = 'CC'
  
  return(ccmodel)
  
}

predict.CC = function(object, newdata){
  
  predictions = list()
  for (lab in object$chain_order) {
    model = ccmodel$models[[lab]]
    probs = posterior_linpred(classifier, transform = TRUE, newdata = newdata)
    pred_lab = as.integer(colMeans(preds) > 0.5)
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
      cols = sample(names(feature), eccmodel$ncol),
      chain_order = sample(names(labels))
    )
  })
  
  eccmodel$models <- parallel::mclapply(seq(m), function(iteration) {
    sub_feature = feature[idx[[iteration]]$rows, idx[[iteration]]$cols]
    chain_order <- idx[[iteration]]$chain_order
    
    ccmodel <- cc(label, sub_feature, chain_order, prior = prior, 
                  prior_intercept = prior_intercept, chains=chains, cores = cores)
    ccmodel$attrs = colnames(sub_feature)
    rm(sub_feature)
    
  }, mc.cores = cores)
  
  class(eccmodel) <- "ECC"
  
  return(eccmodel)
  
}

predict.ECC = function(object, newdata){
  
  all_preds = parallel::mclapply(object$models, function(ccmodel) {
    predict.CC(ccmodel, newdata[, ccmodel$attrs])
  }, mc.cores=cores)
  
  preds_list = lapply(all_preds, function(pred) {
    do.call(cbind, pred)
  }  ) 
  
  results = lcard_threshold(preds_list, object$cardinality)
  
  return(results)
}









