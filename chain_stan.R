## classifier chain using stan

# load package
library(rstan)
library(pROC)

# load standardization function
source("Data_scaling.R")

# import data
X = data.table::fread("ATUS_covariates_new.csv")
labels = data.table::fread("ATUS_sleep24.csv")
X_mat = scaled_data(X)

# stan model
mod_c = stanc("logit.stan")
model = stan_model(stanc_ret = mod_c)

cc = function(label,
              feature,
              chain_order,
              alpha_nu = 5,
              beta_nu = 5,
              chains = 4,
              cores = 4,
              iter = 2000,
              warmup = floor(iter/2), 
              thin = 1){
  
  ccmodel = list(chain_order=chain_order) 
  thresholds = floor(colMeans(label)*100)/100
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

# model = cc(label=labels, feature = X_mat, chain_order = colnames(labels))

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

test = rstan::extract(model$models$V1)
alpha_post = test$alpha
beta_post = test$beta
newdata = X_mat
