//
// This Stan program defines a logistic regression model
// alpha ~ t(5, 0, 10)
// beta ~ t(5, 0, 2.5)

// eta = alpha + x * beta
// rho = inv-logit(eta)
// y ~ bernoulli(rho)


data {
  int<lower=0> N; // data size
  int<lower=0> p; // number of variables
  
  // prior parameters for coefficients
  real alpha_nu;
  real alpha_m;
  real alpha_s;
  vector[p] beta_nu;
  vector[p] beta_m;
  vector[p] beta_s;
  
  // data
  matrix[N, p] X;
  int<lower=0, upper=1> y[N];
}

parameters {
  // intercept
  real alpha;
  
  // covariates
  vector[p] beta;
}

transformed parameters {
  vector[N] eta; // linear predictor
  vector[N] rho; // bernoulli mean
  
  eta = alpha + X * beta;
  rho = inv_logit(eta);
}

model {
  // prior for alpha
  alpha ~ student_t(alpha_nu, alpha_m, alpha_s);
  // prior for beta
  beta ~ student_t(beta_nu, beta_m, beta_s);
  
  // data likelihood
  y ~ bernoulli(rho);
}

