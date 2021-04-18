# import required packages
library(tidyverse)
library(data.table)

scaled_factor = function(x){
  ##
  ## scale a vector that is identified as a factor
  ##
  
  x = ifelse(x == 1, 1-sum(x)/length(x), -sum(x)/length(x))
}

scaled_data = function(data_mat){
  ##
  ## Return scaled original data matrix
  ##
  
  y = rep(0, nrow(data_mat))
  # standardize the data
  f_varname = c("PEEDUCA", "PEMARITL", "PTDTRACE", "SEASON", "CHILD",
                "STATE", "TESEX", "WEEKDAY", "TELFS2")
  f_varname = paste(f_varname, collapse = "|")
  
  fit_formula = paste("y",
                      paste(colnames(data_mat), collapse = " + "),
                      sep = " ~ ")
  # drop the intercept
  X_fit = model.matrix(formula(fit_formula), data = data_mat)[,-1]
  
  # scale continuous variables
  c_var = 0.5 * scale(X_fit[, -grep(f_varname, colnames(X_fit))])
  # scale categorical variables
  f_var = X_fit[, grep(f_varname, colnames(X_fit))]
  f_var = apply(f_var, 2, scaled_factor)
  X_mat = cbind(c_var, f_var)
  
  return(X_mat)
}

X = data.table::fread("ATUS_covariates_new.csv") %>% 
  filter(TUYEAR == 2019)

sleep = data.table::fread("ATUS_sleep_periods.csv") %>% 
  inner_join(., X %>% select(TUCASEID), by = "TUCASEID")

sleep = sleep %>% select(-TUCASEID)
X = X %>% select(-c(TUCASEID, TUYEAR))

idx = seq(1, 1440, 60)
hours = lapply(idx, function(i) sleep[, i:(i+59) ])
hours = lapply(hours, function(col) rowSums(col))
hours = do.call(cbind, hours)
hours = as.data.table(hours>30)
hours = hours*1

X_mat = scaled_data(X)

rm(sleep, idx)