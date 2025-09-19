#############################################################
# 📘 Bayesian Analysis with Conjugate Priors (Normal Data)
# Author: Viriya Gunawan Lim
#
# Goal:
# - Demonstrate conjugate priors for Normal models.
# - Case 1: Variance known, prior on μ.
# - Case 2: Mean & variance both unknown, prior on μ and σ².
#############################################################

# Data: sample observations
y <- c(1.64,1.70,1.72,1.74,1.82,1.82,1.82,1.90,2.08)

# Basic statistics
ybar <- mean(y)      # sample mean
n <- length(y)       # sample size
yvar <- var(y)       # sample variance

ybar; n; yvar

# Sequence of μ values for plotting priors/posteriors
my.seq <- seq(0.5, 3, length = 300)

#############################################################
# 🔹 Case 1: Variance known (σ² = 0.01)
#############################################################

# Prior parameters for μ
my.delta <- 1.9    # prior mean
my.tau   <- 0.3    # prior sd
# Interpreted as being 95% sure that μ is in [1.3, 2.5].

# Prior distribution for μ
prior.mu <- dnorm(my.seq, mean = my.delta, sd = my.tau)

# Posterior distribution for μ (Normal-Normal conjugacy)
posterior.mu <- dnorm(
  my.seq,
  mean = ((my.delta/(my.tau^2) + sum(y)/0.01) /
          (1/(my.tau^2) + n/0.01)),
  sd   = sqrt(0.01 * (my.tau^2) / (0.01 + n * (my.tau^2)))
)

# Plot prior vs posterior
plot(my.seq, prior.mu, ylim = c(-0.7, 12), yaxt = 'n',
     xlab = "Values of μ", ylab = "Probability Distribution",
     type = "l", lty = 3)
lines(my.seq, posterior.mu, col = "blue")

# Posterior point estimates for μ
posterior.mean <- ((my.delta/(my.tau^2) + sum(y)/0.01) /
                  (1/(my.tau^2) + n/0.01))
posterior.median <- qnorm(
  0.50,
  mean = posterior.mean,
  sd   = sqrt(0.01*(my.tau^2)/(0.01+n*(my.tau^2)))
)
print(paste("posterior.mean =", round(posterior.mean, 3),
            "posterior.median =", round(posterior.median, 3)))

# 95% HPD interval for μ
library(TeachingDemos)
hpd.95 <- hpd(qnorm, mean = posterior.mean,
              sd = sqrt(0.01*(my.tau^2)/(0.01+n*(my.tau^2))),
              conf = 0.95)
round(hpd.95, 3)
segments(hpd.95[1], 0, hpd.95[2], 0, lwd = 4)

# Visualization & summary with bayesrules helpers
library(bayesrules)
plot_normal_normal(mean = 1.9, sd = 0.3,
                   sigma = sqrt(0.01),
                   y_bar = ybar, n = n)
summarize_normal_normal(mean = 1.9, sd = 0.3,
                        sigma = sqrt(0.01),
                        y_bar = ybar, n = n)

#############################################################
# 🔹 Case 2: Mean and variance both unknown
#############################################################

# Prior parameters for inverse gamma prior on σ²
my.alpha <- 0.18
my.beta  <- 0.34

# Prior parameters for μ
my.delta <- 1.9
s0 <- 1

library(pscl)

# Posterior mean for σ²
p.mean.sig.sq <- (my.beta + 0.5 * (sum(y^2) - n * (ybar^2))) /
                 (my.alpha + n/2 - 0.5 - 1)

# Posterior median for σ²
p.median.sig.sq <- qigamma(0.5,
                           my.alpha + n/2 - 0.5,
                           my.beta + 0.5 * (sum(y^2) - n * (ybar^2)))

# 95% HPD interval for σ²
hpd.95.sig.sq <- hpd(qigamma,
                     alpha = my.alpha + n/2 - 0.5,
                     beta  = my.beta + 0.5*(sum(y^2) - n*(ybar^2)))

p.mean.sig.sq; p.median.sig.sq; hpd.95.sig.sq

#############################################################
# ✅ Conclusion:
# - Case 1: Normal-Normal conjugacy gives closed-form posterior for μ.
# - Case 2: When variance unknown, we use inverse-gamma prior for σ²
#   and derive posterior moments & intervals.
# - The framework shows how prior beliefs update with observed data.
#############################################################
