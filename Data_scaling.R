# import required packages
library(tidyverse)
library(data.table)

scaled_factor = function(x){
  ##
  ## scale a vector that is identified as a factor
  ##
  
  x = ifelse(x == 1, 1-sum(x)/length(x), -sum(x)/length(x))
  return(x)
}

scaled_data = function(data_mat){
  ##
  ## Return scaled original data matrix
  ##
  
  y = rep(0, nrow(data_mat))
  # standardize the data
  c_varname = c("UNEMPLOYRATE", "TEAGE")
  c_varname = paste(c_varname, collapse = "|")
  
  fit_formula = paste("y",
                      paste(colnames(data_mat), collapse = " + "),
                      sep = " ~ ")
  # drop the intercept
  X_fit = model.matrix(formula(fit_formula), data = data_mat)[,-1]
  
  # scale continuous variables
  c_var = 0.5 * scale(X_fit[, grep(c_varname, colnames(X_fit))])
  # scale categorical variables
  f_var = X_fit[, -grep(c_varname, colnames(X_fit))]
  f_var = apply(f_var, 2, scaled_factor)
  X_mat = cbind(c_var, f_var)
  
  return(X_mat)
}