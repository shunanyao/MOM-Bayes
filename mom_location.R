library(cmdstanr)
library(posterior)
library(bayesplot)
library("ggplot2")
library(loo)



################### location model, no outliers ####################
rm(list = ls())



file <- file.path("~/Documents/mom_bayes", "mom_location.stan")
mod <- cmdstan_model(file)

M = 5
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(20, 40, 60, 80, 100)
a = 2
mu_prior = 0
sigma_prior = 10
log_lik = matrix(0, nrow = M, ncol = NOP)
posterior_vec = c()
x_vec = c()


for (j in 1 : M) {
  # running MCMC
  set.seed(20241005)
  k = k_vec[j]
  n = floor(1000 / k)
  theta_0 = -29.5
  x = rnorm(k * n, theta_0, 1)
  x = sample(x, length(x), replace = FALSE)
  mom_data = list(
    a = a,
    k = k,                          # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior,
    initial_theta = 0
  )
  fit <- mod$sample(
    data = mom_data,
    chains = 4,
    parallel_chains = 4,
    refresh = 1000, # print update every 1000 iters
    iter_warmup = 2000,
    iter_sampling = 10000
  )
  draws_df <- fit$draws(format = "df")
  posterior = as.vector(draws_df$theta)
  # posterior_vec is where the samples are stored
  posterior_vec = c(posterior_vec, posterior)
}

save.image("location_model_no_outlier_likelihood.RData")



################### location model, outliers ####################
rm(list = ls())

file <- file.path("~/Documents/mom_bayes", "mom_location.stan")
mod <- cmdstan_model(file)

M = 5
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(20, 40, 60, 80, 100)
a = 2
mu_prior = 0
sigma_prior = 10
log_lik = matrix(0, nrow = M, ncol = NOP)
posterior_vec = c()
x_vec = c()


for (j in 1 : M) {
  # running MCMC
  set.seed(20241004)
  k = k_vec[j]
  n = floor(1000 / k)
  theta_0 = -29.5
  x = c(rnorm(k * n - 40, theta_0, 1), rnorm(40, 10000, 1))
  x = sample(x, length(x), replace = FALSE)
  mom_data = list(
    a = a,
    k = k,                          # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior,
    initial_theta = 0
  )
  fit <- mod$sample(
    data = mom_data,
    chains = 4,
    parallel_chains = 4,
    refresh = 1000, # print update every 1000 iters
    iter_warmup = 2000,
    iter_sampling = 10000
  )
  draws_df <- fit$draws(format = "df")
  posterior = as.vector(draws_df$theta)
  # posterior_vec is where the samples are stored
  posterior_vec = c(posterior_vec, posterior)
}

save.image("location_model_outliers_likelihood.RData")