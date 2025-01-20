library(bcogsci)
library(cmdstanr)
library(rstan)
library(purrr)
library(dplyr)

rm(list = ls())

seed = 20241105

set.seed(seed)


a = 2
k = 20
n = 50
ind = dim(df_dots)[1]
ind = sample(1 : ind, n * k)
dat = df_dots[ind,]
x = dat$diff
x[x == "easy"] = -0.5
x[x == "hard"] = 0.5
x = as.numeric(x)
N = length(x)
rt = dat$rt

# Next three lines are for adding outliers. Uncomment for outliers.
# ind = rep(0, n * k)
# ind[sample(1 : n * k, 10, replace = FALSE)] = 1
# rt[ind == 1] = exp(rnorm(10, 100, 1))


file <- file.path("~/Documents/mom_bayes", "mom_mixture.stan")
mod <- cmdstan_model(file)


mixture_data = list(
  k = k,
  n = n,
  x = x,
  rt = rt,
  a = a,
  initial_alpha = 0.5,
  initial_beta = 0.5,
  initial_gamma = 0.75,
  initial_p_task = 0.5,
  initial_sigma = 0.22,
  initial_sigma2 = 0.41,
  sigma = 0.22,
  sigma2 = 0.41
)

fit <- mod$sample(
  data = mixture_data,
  seed = seed,
  chains = 4,
  parallel_chains = 4,
  refresh = 500, # print update every 500 iters
  iter_warmup = 2000,
  iter_sampling = 10000
)

draws_df <- fit$draws(format = "df")
parameter_matrix = as.matrix(draws_df)[,2:5]
save.image("standard_mixture_outliers.RData")




