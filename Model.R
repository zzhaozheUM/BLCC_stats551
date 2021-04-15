# import required packages
library(rstan)
library(tidyverse)

# import data
sleep = data.table::fread("/home/xlyu/atus/ATUS_sleep_periods.csv")
rownames(sleep) = sleep$TUCASEID
sleep = subset(sleep, select = -TUCASEID)
X = data.table::fread("/home/xlyu/atus/ATUS_covariates_new.csv")
rownames(X) = X$TUCASEID
X = subset(X, select = -TUCASEID)

y = as.vector(unlist(sleep[,1]))

# standardize the data
f_varname = c("PEEDUCA", "PEMARITL", "PTDTRACE", "SEASON", "CHILD", 
              "STATE", "TESEX", "WEEKDAY", "TELFS2")
f_varname = paste(f_varname, collapse = "|")

fit_formula = y ~ TELFS2 + UNEMPLOYRATE + TEAGE + PEEDUCA + PEMARITL + PTDTRACE +
  TUYEAR + CHILD + TESEX + SEASON + STATE + WEEKDAY
X_fit = model.matrix(fit_formula, data = X)[,-1] # drop the intercept

# scale continuous variables
c_var = 0.5 * scale(X_fit[, -grep(f_varname, colnames(X_fit))])
# scale categorical variables
f_var = X_fit[, grep(f_varname, colnames(X_fit))]
f_var = apply(f_var, 2, function(x) {
  x = ifelse(x == 1, 1-sum(x)/length(x), -sum(x)/length(x))})

X_mat = cbind(c_var, f_var)


# data for stan
data = list(N = nrow(X),
            p = dim(X_mat)[2],
            alpha_nu = 5,
            alpha_m = 0,
            alpha_s = 10,
            beta_nu = rep(5, dim(X_mat)[2]),
            beta_m = rep(0, dim(X_mat)[2]),
            beta_s = rep(2.5, dim(X_mat)[2]),
            X = X_mat,
            y = y)

mod_c = stanc("/home/xlyu/stats551/logit.stan")
model = stan_model(stanc_ret = mod_c)

current = Sys.time()
fit_logit = sampling(model, data = data, cores = 4)
print(fit_logit, c("beta")) # check convergence and estimated coefficients
time.fit = Sys.time() - current
