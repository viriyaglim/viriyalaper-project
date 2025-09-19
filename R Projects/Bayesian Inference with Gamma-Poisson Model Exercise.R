#############################################################
# 📘 Bayesian Inference with Gamma-Poisson Model
# Exercises 6.15 & 6.16
# Author: Viriya Gunawan Lim
#
# Using Stan to fit Gamma-Poisson models:
# - Exercise 6.15: Prior Gamma(20, 5)
# - Exercise 6.16: Prior Gamma(5, 5)
# Compare posteriors of λ given observed data Y = (0, 1, 0).
#############################################################

#Exercise 6.15
# Load required library
library(rstan)
library(mcmcplots)
library(bayesplot)
library(ggplot2)
library(bayesrules)

gp_model <- "
data {
int<lower = 0> Y[3];
}
parameters {
real<lower = 0> lambda;
}
model {
Y ~ poisson(lambda);
lambda ~ gamma(20, 5);
}
"
# STEP 2: SIMULATE the posterior
gp_sim <- stan(model_code = gp_model, data = list(Y = c(0,1,0)),
               chains = 4, iter = 5000*2, seed = 84735)

print(gp_sim)
plot <- as.array(gp_sim, pars = "lambda")[, , 1]
mcmc_trace(plot)
# Trace plots of the 4 Markov chains
mcmc_trace(gp_sim, pars = "lambda", size = 0.1)
# Histogram of the Markov chain values
mcmc_hist(gp_sim, pars = "lambda") +
  yaxis_text(TRUE) +
  ylab("count")
# Density plot of the Markov chain values
mcmc_dens(gp_sim, pars = "lambda") +
  yaxis_text(TRUE) +
  ylab("density")
dens <- as.array(gp_sim, pars = "lambda")[, , 1]
mcmc_dens(dens)
colMeans(as.array(gp_sim)[,,1])
summarize_gamma_poisson(20,5,sum_y=1,n=3)


#Exercise 6.16

gp_model <- "
data {
int<lower = 0> Y[3];
}
parameters {
real<lower = 0> lambda;
}
model {
Y ~ poisson(lambda);
lambda ~ gamma(5, 5);
}
"
# STEP 2: SIMULATE the posterior
gp_sim <- stan(model_code = gp_model, data = list(Y = c(0,1,0)),
               chains = 4, iter = 5000*2, seed = 84735)

print(gp_sim)
# Trace plots of the 4 Markov chains
mcmc_trace(gp_sim, pars = "lambda", size = 0.1)

plot <- as.array(gp_sim, pars = "lambda")[, , 1]
mcmc_trace(plot)
# Histogram of the Markov chain values
mcmc_hist(gp_sim, pars = "lambda") +
  yaxis_text(TRUE) +
  ylab("count")
# Density plot of the Markov chain values
mcmc_dens(gp_sim, pars = "lambda") +
  yaxis_text(TRUE) +
  ylab("density")
dens <- as.array(gp_sim, pars = "lambda")[, , 1]
mcmc_dens(dens)
colMeans(as.array(gp_sim)[,,1])
summarize_gamma_poisson(5,5,sum_y=1,n=3)

