# MOM-Bayes

The codes used for paper ''Median of Means Principle for Bayesian Inference''. All codes are run on R.

To run the simulation setting in the paper, please run mom_sim.R. Inside the file, there are options for clean/contaminated scenario and different prior distributions. Also, users have the choice to compare robust posterior using Huber's loss (using the file mom_posterior.stan inside stan function), robust posterior using median (using mom_median.stan), and standard posterior (using normal.stan).

To run the real data analysis, please run
