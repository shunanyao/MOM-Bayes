library(cmdstanr)
library(posterior)
library(bayesplot)
library("ggplot2")
library(gridExtra)
library(grid)
library(lattice)
# library(latex2exp)
library(leaps)

rm(list = ls())

set.seed(20241003)

d = read.csv2('wine.csv', header = TRUE)
for (i in 1 : 12) {
  d[,i] = as.numeric(d[,i])
}
d_mean = colMeans(d)
d_mean = rep(d_mean, 4898)
d_mean = matrix(d_mean, nrow = 12, ncol = 4898)
d_mean = t(d_mean)
d_var = var(d)
d_var = diag(d_var)
d_var = rep(d_var, 4898)
d_var = matrix(d_var, nrow = 12, ncol = 4898)
d_var = t(d_var)
d_var = sqrt(d_var)
d = d - d_mean
d = d / d_var
ind_shuffle = 1 : 4898
ind_shuffle = sample(ind_shuffle, 4898)
d = d[ind_shuffle,]
vars = c(1,2,4,6,8,9,10,11)
x_data = d[,vars]
x_data = cbind(intercept = 1, x_data)
y_data = d[,12]
# The next three lines are for adding outliers. Uncomment for outliers
# ind = rep(0, 4898)
# ind[sample(1 : 4898, 10, replace = FALSE)] = 1
# y_data[ind == 1] = rnorm(10, 1000, 10)



k = 31
n = floor(4898 / k)
dim = dim(x_data)[2]
a = sqrt(n)

file <- file.path("~/Documents/mom_bayes", "mom_wine.stan")
mod <- cmdstan_model(file)



wine_data = list(
  a = a,
  k = k,
  n = n,                                   
  dim = dim,
  x = x_data,
  y = y_data,
  initial_theta = rep(0, dim),
  initial_sigma = 5
)

fit <- mod$sample(
  data = wine_data,
  seed = 20241027,
  chains = 4,
  parallel_chains = 4,
  refresh = 10, # print update every 1000 iters
  iter_warmup = 2000,
  iter_sampling = 10000,
  init = list(
    list(theta = rep(0, dim), sigma = 1),
    list(theta = rep(0, dim), sigma = 1),
    list(theta = rep(0, dim), sigma = 1),
    list(theta = rep(0, dim), sigma = 1)
  )
)

draws_df <- fit$draws(format = "df")
parameter_matrix = as.matrix(draws_df)
parameter_matrix = parameter_matrix[,2:11]


save.image("mom_wine_outliers.RData")
