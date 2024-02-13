library(rstan)
library("bayesplot")
library("ggplot2")

rm(list = ls())
# 
# 
# 
# 

rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = 0
sigma_prior = 1
posterior_vec = c()



for (j in 1 : M) {
  
  k = k_vec[j]
  n = floor(1000 / k)
  
  
  theta_0 = -30
  theta_vec[j] = theta_0
  half_k = floor(k / 2)
  x = c(rnorm(k * n, theta_0, 1))
  x = sample(x, length(x), replace = FALSE)
  mom_data = list(
    a = a,
    k = k,                            # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior
  )
  fit1 <- stan(
    file = "C:/Users/lenovo/Desktop/mom_2_median.stan",  # path to mom.stan
    data = mom_data,        # named list of data
    chains = 4,             # number of Markov chains
    warmup = 2000,          # number of warmup iterations per chain
    iter = 12000,            # total number of iterations per chain
    cores = 3,              # number of cores (could use one per chain)
    refresh = 0             # no progress shown
  )
  # stats[j,] = summary(fit1)$summary[3,]
  stats_summary = summary(fit1)[[1]]
  sd = stats_summary[1,3]
  asym_sd = sqrt(k * n) * sd
  asym_sd_vec[j] = asym_sd
  zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
  zero_in_ci_vec[j] = zero_in_ci
  posterior = as.vector(as.matrix(fit1)[,1])
  posterior_vec = c(posterior_vec, posterior)
  # # # colnames(posterior) = c('theta')
  # # posterior_plot = mcmc_areas(posterior, prob = 0.95)
  # sigma_square = 1 / ((1 / sigma_prior^2) + (k * n / 1))
  # mu = sigma_square * (mu_prior / sigma_prior^2 + sum(x) / 1)
  # sigma = sqrt(sigma_square)
  # 
  # q1 = quantile(posterior, 0.025)
  # q2 = quantile(posterior, 0.975)
  # df = as.data.frame(posterior)
  # p = ggplot(df, aes(x=posterior)) + geom_density() + geom_density(color="blue")
  # d <- ggplot_build(p)$data[[1]]
  # p = p + geom_area(data = subset(d, x > q1 & x < q2), aes(x=x, y=y), fill="deepskyblue1", alpha = 0.2)
  # p = p + geom_vline(aes(xintercept=mean(posterior)), color="darkblue", linetype="solid", size=1)
  # p = p + stat_function(fun = dnorm, args = list(mu, sigma), color = 'red') + stat_function(fun=funcShaded, args = list(mu, sigma), geom="area", fill="tomato", alpha=0.2)
  # 
  # posterior_vec[[j]] = p
}

save.image("C:/Users/lenovo/Desktop/MoM/1000_strong_wrong_belief_2_m.RData")



rm(list = ls())
# 
# 
# 
# 

rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = 0
sigma_prior = 1
posterior_vec = c()



for (j in 1 : M) {

  k = k_vec[j]
  n = floor(1000 / k)


  theta_0 = -30
  theta_vec[j] = theta_0
  half_k = floor(k / 2)
  x = c(rnorm(k * n - 40, theta_0, 1), rnorm(40, 10000, 1))
  x = sample(x, length(x), replace = FALSE)
  mom_data = list(
    a = a,
    k = k,                            # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior
  )
  fit1 <- stan(
    file = "C:/Users/lenovo/Desktop/mom_2_median.stan",  # path to mom.stan
    data = mom_data,        # named list of data
    chains = 4,             # number of Markov chains
    warmup = 2000,          # number of warmup iterations per chain
    iter = 12000,            # total number of iterations per chain
    cores = 3,              # number of cores (could use one per chain)
    refresh = 0             # no progress shown
  )
  # stats[j,] = summary(fit1)$summary[3,]
  stats_summary = summary(fit1)[[1]]
  sd = stats_summary[1,3]
  asym_sd = sqrt(k * n) * sd
  asym_sd_vec[j] = asym_sd
  zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
  zero_in_ci_vec[j] = zero_in_ci
  posterior = as.vector(as.matrix(fit1)[,1])
  posterior_vec = c(posterior_vec, posterior)
  # # # colnames(posterior) = c('theta')
  # # posterior_plot = mcmc_areas(posterior, prob = 0.95)
  # sigma_square = 1 / ((1 / sigma_prior^2) + (k * n / 1))
  # mu = sigma_square * (mu_prior / sigma_prior^2 + sum(x) / 1)
  # sigma = sqrt(sigma_square)
  # 
  # q1 = quantile(posterior, 0.025)
  # q2 = quantile(posterior, 0.975)
  # df = as.data.frame(posterior)
  # p = ggplot(df, aes(x=posterior)) + geom_density() + geom_density(color="blue")
  # d <- ggplot_build(p)$data[[1]]
  # p = p + geom_area(data = subset(d, x > q1 & x < q2), aes(x=x, y=y), fill="deepskyblue1", alpha = 0.2)
  # p = p + geom_vline(aes(xintercept=mean(posterior)), color="darkblue", linetype="solid", size=1)
  # p = p + stat_function(fun = dnorm, args = list(mu, sigma), color = 'red') + stat_function(fun=funcShaded, args = list(mu, sigma), geom="area", fill="tomato", alpha=0.2)
  # 
  # posterior_vec[[j]] = p
}

save.image("C:/Users/lenovo/Desktop/MoM/1000_strong_wrong_belief_with_outliers_2_m.RData")

rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = 0
sigma_prior = 10
posterior_vec = c()

for (j in 1 : M) {

  k = k_vec[j]
  n = floor(1000 / k)


  theta_0 = -30
  theta_vec[j] = theta_0
  x = c(rnorm(k * n, theta_0, 1))
  mom_data = list(
    a = a,
    k = k,                            # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior
  )
  fit1 <- stan(
    file = "C:/Users/lenovo/Desktop/mom_2_median.stan",  # path to mom.stan
    data = mom_data,        # named list of data
    chains = 4,             # number of Markov chains
    warmup = 2000,          # number of warmup iterations per chain
    iter = 12000,            # total number of iterations per chain
    cores = 2,              # number of cores (could use one per chain)
    refresh = 0             # no progress shown
  )
  # stats[j,] = summary(fit1)$summary[3,]
  stats_summary = summary(fit1)[[1]]
  sd = stats_summary[1,3]
  asym_sd = sqrt(k * n) * sd
  asym_sd_vec[j] = asym_sd
  zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
  zero_in_ci_vec[j] = zero_in_ci
  posterior = as.vector(as.matrix(fit1)[,1])
  posterior_vec = c(posterior_vec, posterior)
  # colnames(posterior) = c('theta')



  # posterior_plot = mcmc_areas(posterior, prob = 0.95)
  # sigma_square = 1 / ((1 / sigma_prior^2) + (k * n / 1))
  # mu = sigma_square * (mu_prior / sigma_prior^2 + sum(x) / 1)
  # sigma = sqrt(sigma_square)
  #
  # q1 = quantile(posterior, 0.025)
  # q2 = quantile(posterior, 0.975)
  # df = as.data.frame(posterior)
  # p = ggplot(df, aes(x=posterior)) + geom_density() + geom_density(color="blue")
  # d <- ggplot_build(p)$data[[1]]
  # p = p + geom_area(data = subset(d, x > q1 & x < q2), aes(x=x, y=y), fill="deepskyblue1", alpha = 0.2)
  # p = p + geom_vline(aes(xintercept=mean(posterior)), color="darkblue", linetype="solid", size=1)
  # p = p + stat_function(fun = dnorm, args = list(mu, sigma), color = 'red') + stat_function(fun=funcShaded, args = list(mu, sigma), geom="area", fill="tomato", alpha=0.2)
  #
  # posterior_vec[[j]] = p
}

save.image("C:/Users/lenovo/Desktop/MoM/1000_weak_wrong_belief_2_m.RData")
#
#
#
#
rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = 0
sigma_prior = 10
posterior_vec = c()


for (j in 1 : M) {

  k = k_vec[j]
  n = floor(1000 / k)


  theta_0 = -30
  theta_vec[j] = theta_0
  half_k = floor(k / 2)
  x = c(rnorm(k * n - 40, theta_0, 1), rnorm(40, 10000, 1))
  x = sample(x, length(x), replace = FALSE)
  mom_data = list(
    a = a,
    k = k,                            # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior
  )
  fit1 <- stan(
    file = "C:/Users/lenovo/Desktop/mom_2_median.stan",  # path to mom.stan
    data = mom_data,        # named list of data
    chains = 4,             # number of Markov chains
    warmup = 2000,          # number of warmup iterations per chain
    iter = 12000,            # total number of iterations per chain
    cores = 2,              # number of cores (could use one per chain)
    refresh = 0             # no progress shown
  )
  # stats[j,] = summary(fit1)$summary[3,]
  stats_summary = summary(fit1)[[1]]
  sd = stats_summary[1,3]
  asym_sd = sqrt(k * n) * sd
  asym_sd_vec[j] = asym_sd
  zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
  zero_in_ci_vec[j] = zero_in_ci
  posterior = as.vector(as.matrix(fit1)[,1])
  posterior_vec = c(posterior_vec, posterior)
  # colnames(posterior) = c('theta')



  # posterior_plot = mcmc_areas(posterior, prob = 0.95)
  # sigma_square = 1 / ((1 / sigma_prior^2) + (k * n / 1))
  # mu = sigma_square * (mu_prior / sigma_prior^2 + sum(x) / 1)
  # sigma = sqrt(sigma_square)
  #
  # q1 = quantile(posterior, 0.025)
  # q2 = quantile(posterior, 0.975)
  # df = as.data.frame(posterior)
  # p = ggplot(df, aes(x=posterior)) + geom_density() + geom_density(color="blue")
  # d <- ggplot_build(p)$data[[1]]
  # p = p + geom_area(data = subset(d, x > q1 & x < q2), aes(x=x, y=y), fill="deepskyblue1", alpha = 0.2)
  # p = p + geom_vline(aes(xintercept=mean(posterior)), color="darkblue", linetype="solid", size=1)
  # p = p + stat_function(fun = dnorm, args = list(mu, sigma), color = 'red') + stat_function(fun=funcShaded, args = list(mu, sigma), geom="area", fill="tomato", alpha=0.2)
  #
  # posterior_vec[[j]] = p
}

save.image("C:/Users/lenovo/Desktop/MoM/1000_weak_wrong_belief_with_outliers_2_m.RData")

rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = -29.50
sigma_prior = 1
posterior_vec = c()


for (j in 1 : M) {
  
  k = k_vec[j]
  n = floor(1000 / k)
  
  
  theta_0 = -30
  theta_vec[j] = theta_0
  half_k = floor(k / 2)
  x = c(rnorm(k * n, theta_0, 1))
  x = sample(x, size = length(x), replace = FALSE)
  mom_data = list(
    a = a,
    k = k,                            # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior
  )
  fit1 <- stan(
    file = "C:/Users/lenovo/Desktop/mom_median.stan",  # path to mom.stan
    data = mom_data,        # named list of data
    chains = 4,             # number of Markov chains
    warmup = 2000,          # number of warmup iterations per chain
    iter = 12000,            # total number of iterations per chain
    cores = 2,              # number of cores (could use one per chain)
    refresh = 0             # no progress shown
  )
  # stats[j,] = summary(fit1)$summary[3,]
  stats_summary = summary(fit1)[[1]]
  sd = stats_summary[1,3]
  asym_sd = sqrt(k * n) * sd
  asym_sd_vec[j] = asym_sd
  zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
  zero_in_ci_vec[j] = zero_in_ci
  posterior = as.vector(as.matrix(fit1)[,1])
  posterior_vec = c(posterior_vec, posterior)
  # colnames(posterior) = c('theta')
  
  
  
  # posterior_plot = mcmc_areas(posterior, prob = 0.95)
  # sigma_square = 1 / ((1 / sigma_prior^2) + (k * n / 1))
  # mu = sigma_square * (mu_prior / sigma_prior^2 + sum(x) / 1)
  # sigma = sqrt(sigma_square)
  #
  # q1 = quantile(posterior, 0.025)
  # q2 = quantile(posterior, 0.975)
  # df = as.data.frame(posterior)
  # p = ggplot(df, aes(x=posterior)) + geom_density() + geom_density(color="blue")
  # d <- ggplot_build(p)$data[[1]]
  # p = p + geom_area(data = subset(d, x > q1 & x < q2), aes(x=x, y=y), fill="deepskyblue1", alpha = 0.2)
  # p = p + geom_vline(aes(xintercept=mean(posterior)), color="darkblue", linetype="solid", size=1)
  # p = p + stat_function(fun = dnorm, args = list(mu, sigma), color = 'red') + stat_function(fun=funcShaded, args = list(mu, sigma), geom="area", fill="tomato", alpha=0.2)
  #
  # posterior_vec[[j]] = p
}

save.image("C:/Users/lenovo/Desktop/MoM/1000_correct_belief_m.RData")






rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = -29.50
sigma_prior = 1
posterior_vec = c()


for (j in 1 : M) {

  k = k_vec[j]
  n = floor(1000 / k)


  theta_0 = -30
  theta_vec[j] = theta_0
  half_k = floor(k / 2)
  x = c(rnorm(k * n - 40, theta_0, 1), rnorm(40, 10000, 1))
  x = sample(x, size = length(x), replace = FALSE)
  mom_data = list(
    a = a,
    k = k,                            # number of blocks
    n = n,                          # number of samples each block
    x = x,                          # data
    mu_prior = mu_prior,
    sigma_prior = sigma_prior
  )
  fit1 <- stan(
    file = "C:/Users/lenovo/Desktop/mom_2_median.stan",  # path to mom.stan
    data = mom_data,        # named list of data
    chains = 4,             # number of Markov chains
    warmup = 2000,          # number of warmup iterations per chain
    iter = 12000,            # total number of iterations per chain
    cores = 2,              # number of cores (could use one per chain)
    refresh = 0             # no progress shown
  )
  # stats[j,] = summary(fit1)$summary[3,]
  stats_summary = summary(fit1)[[1]]
  sd = stats_summary[1,3]
  asym_sd = sqrt(k * n) * sd
  asym_sd_vec[j] = asym_sd
  zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
  zero_in_ci_vec[j] = zero_in_ci
  posterior = as.vector(as.matrix(fit1)[,1])
  posterior_vec = c(posterior_vec, posterior)
  # colnames(posterior) = c('theta')



  # posterior_plot = mcmc_areas(posterior, prob = 0.95)
  # sigma_square = 1 / ((1 / sigma_prior^2) + (k * n / 1))
  # mu = sigma_square * (mu_prior / sigma_prior^2 + sum(x) / 1)
  # sigma = sqrt(sigma_square)
  #
  # q1 = quantile(posterior, 0.025)
  # q2 = quantile(posterior, 0.975)
  # df = as.data.frame(posterior)
  # p = ggplot(df, aes(x=posterior)) + geom_density() + geom_density(color="blue")
  # d <- ggplot_build(p)$data[[1]]
  # p = p + geom_area(data = subset(d, x > q1 & x < q2), aes(x=x, y=y), fill="deepskyblue1", alpha = 0.2)
  # p = p + geom_vline(aes(xintercept=mean(posterior)), color="darkblue", linetype="solid", size=1)
  # p = p + stat_function(fun = dnorm, args = list(mu, sigma), color = 'red') + stat_function(fun=funcShaded, args = list(mu, sigma), geom="area", fill="tomato", alpha=0.2)
  #
  # posterior_vec[[j]] = p
}

save.image("C:/Users/lenovo/Desktop/MoM/1000_correct_belief_with_outliers_2_m.RData")





rm(list = ls())

set.seed(20210236)


N = 1000
theta_0 = -30
mu_prior = 100
sigma_prior = 1
x = c(rnorm(1000 - 40, theta_0, 1), rnorm(40, 1000, 1))
x = sample(x, size = length(x), replace = FALSE)
mom_data = list(
  N = N, # number of samples each block
  x = x,
  mu_prior = mu_prior,
  sigma_prior = sigma_prior
)
fit1 <- stan(
  file = "C:/Users/lenovo/Desktop/rho_bayes.stan",  # path to mom.stan
  data = mom_data,        # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 12000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
# stats[j,] = summary(fit1)$summary[3,]
stats_summary = summary(fit1)[[1]]
sd = stats_summary[1,3]
asym_sd = sqrt(N) * sd
zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
posterior = as.vector(as.matrix(fit1)[,1])

save.image("C:/Users/lenovo/Desktop/MoM/rho_bayes.RData")




rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = 100
sigma_prior = 10
posterior_vec = c()


j = 10
k = k_vec[j]
n = floor(1000 / k)


theta_0 = -30
theta_vec[j] = theta_0
half_k = floor(k / 2)
x = c(rnorm(k * n - 40, theta_0, 1), rnorm(40, 1000, 1))
x = sample(x, size = length(x), replace = FALSE)
mom_data = list(
  a = a,
  k = k,                            # number of blocks
  n = n,                          # number of samples each block
  x = x,                          # data
  mu_prior = mu_prior,
  sigma_prior = sigma_prior
)
fit1 <- stan(
  file = "C:/Users/lenovo/Desktop/mom.stan",  # path to mom.stan
  data = mom_data,        # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 12000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
# stats[j,] = summary(fit1)$summary[3,]
stats_summary = summary(fit1)[[1]]
sd = stats_summary[1,3]
asym_sd = sqrt(k * n) * sd
asym_sd_vec[j] = asym_sd
zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
zero_in_ci_vec[j] = zero_in_ci
posterior = as.vector(as.matrix(fit1)[,1])
posterior_vec = c(posterior_vec, posterior)
# colnames(posterior) = c('theta')

save.image("C:/Users/lenovo/Desktop/MoM/mom_1.RData")





rm(list = ls())

set.seed(20210226)

funcShaded <- function(x, mu, sigma) {
  y <- dnorm(x, mean = mu, sd = sigma)
  y[x < mu - 1.96 * sigma | x > mu + 1.96 * sigma] <- NA
  return(y)
}

M = 10
theta_vec = rep(0, M)
asym_sd_vec = rep(0, M)
zero_in_ci_vec = rep(TRUE, M)
posterior_vec = list()
k_vec = c(10, 20, 30, 40, 50, 60, 70, 80, 90, 100)
a = 2
mu_prior = 100
sigma_prior = 10
posterior_vec = c()


j = 10
k = k_vec[j]
n = floor(1000 / k)


theta_0 = -30
theta_vec[j] = theta_0
half_k = floor(k / 2)
x = c(rnorm(k * n - 40, theta_0, 1), rnorm(40, 1000, 1))
x = sample(x, size = length(x), replace = FALSE)
mom_data = list(
  a = a,
  k = k,                            # number of blocks
  n = n,                          # number of samples each block
  x = x,                          # data
  mu_prior = mu_prior,
  sigma_prior = sigma_prior
)
fit1 <- stan(
  file = "C:/Users/lenovo/Desktop/mom_2.stan",  # path to mom.stan
  data = mom_data,        # named list of data
  chains = 4,             # number of Markov chains
  warmup = 2000,          # number of warmup iterations per chain
  iter = 12000,            # total number of iterations per chain
  cores = 2,              # number of cores (could use one per chain)
  refresh = 0             # no progress shown
)
# stats[j,] = summary(fit1)$summary[3,]
stats_summary = summary(fit1)[[1]]
sd = stats_summary[1,3]
asym_sd = sqrt(k * n) * sd
asym_sd_vec[j] = asym_sd
zero_in_ci = (0 >= stats_summary[1,4]) & (0 <= stats_summary[1,8])
zero_in_ci_vec[j] = zero_in_ci
posterior = as.vector(as.matrix(fit1)[,1])
posterior_vec = c(posterior_vec, posterior)
# colnames(posterior) = c('theta')

save.image("C:/Users/lenovo/Desktop/MoM/mom_2.RData")





df = as.data.frame(posterior)
p = ggplot(df, aes(x=posterior)) + geom_density() + geom_density(color="blue")
d <- ggplot_build(p)$data[[1]]
q1 = quantile(posterior, 0.025)
q2 = quantile(posterior, 0.975)
p = p + geom_area(data = subset(d, x > q1 & x < q2), aes(x=x, y=y), fill="deepskyblue1", alpha = 0.2)
# sigma_square = 1 / ((1 / sigma_prior^2) + (1000 / 1))
# mu = sigma_square * (mu_prior / sigma_prior^2 + sum(x) / 1)
# sigma = sqrt(sigma_square)
# p = p + stat_function(fun = dnorm, args = list(mu, sigma), color = 'red', linetype = 'dashed') + stat_function(fun=funcShaded, args = list(mu, sigma), geom="area", fill="tomato", alpha=0.2)
p = p + ylab('posterior')
# p = p + xlab(paste('block number = ', as.character(k_vec)[j], sep = ' '))
p = p + theme_minimal()

print(p)
