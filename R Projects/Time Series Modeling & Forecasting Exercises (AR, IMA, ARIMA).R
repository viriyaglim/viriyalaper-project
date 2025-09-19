#############################################################
# 📊 Time Series Modeling & Forecasting Exercises (R)
# Author: Viriya Gunawan Lim
#
# This script covers exercises from time series analysis:
# - AR(1) vs IMA(1,1) model comparison (Robot data)
# - Asymptotic distribution & bootstrap of AR(1) estimator (Color data)
# - CO2 trend-seasonal regression
# - ARIMA model for Air Passenger data with seasonal differencing
#############################################################

library(forecast)
library(tseries)
library(TSA)
library(boot)

#7.29 
# Load the data
data()
data(robot) # assuming the data is in the 'datasets' package

# Inspect the data
head(robot)
summary(robot)

# Convert data to time series object
robot_ts <- ts(robot)

# Part (a): Estimate the parameters of an AR(1) model
ar1_model <- arima(robot_ts, order=c(1,0,0))

# Print the results
summary(ar1_model)

# Part (b): Estimate the parameters of an IMA(1,1) model
ima11_model <- arima(robot_ts, order=c(0,1,1))

# Print the results
summary(ima11_model)

# Part (c): Compare the results using AIC
ar1_aic <- AIC(ar1_model)
ima11_aic <- AIC(ima11_model)

# Print AIC values
cat("AIC of AR(1) model:", ar1_aic, "\n")
cat("AIC of IMA(1,1) model:", ima11_aic, "\n")

# Conclusion on which model is better
if (ar1_aic < ima11_aic) {
  cat("The AR(1) model is better based on AIC.\n")
} else {
  cat("The IMA(1,1) model is better based on AIC.\n")
}

#7.32
# Load the data
data(color) # assuming the data is in the 'datasets' package

# Inspect the data
head(color)
summary(color)

# Convert data to time series object
color_ts <- ts(color)

# Part 1: Fit an AR(1) model
ar1_model <- arima(color_ts, order=c(1,0,0))

# Print the results
summary(ar1_model)

# Extract the phi estimate
phi_hat <- ar1_model$coef[1]
cat("Estimated phi:", phi_hat, "\n")

# Part 2: Theoretical asymptotic distribution of the estimator of phi
# The standard error of phi in AR(1) is approximately sqrt(1/n)
n <- length(color_ts)
theoretical_se <- sqrt(1/n)
cat("Theoretical SE of phi:", theoretical_se, "\n")

# Part 3: Bootstrap to estimate the distribution of phi
# Define the statistic function for bootstrapping
phi_statistic <- function(data, indices) {
  data_boot <- ts(data[indices])
  fit <- arima(data_boot, order=c(1,0,0))
  return(fit$coef[1])
}

# Perform bootstrap
set.seed(123) # for reproducibility
bootstrap_results <- boot(color_ts, statistic=phi_statistic, R=1000)

# Extract bootstrap estimates of phi
bootstrap_phi <- bootstrap_results$t

bootstrap_phi

# Calculate bootstrap standard error
bootstrap_se <- sd(bootstrap_phi)
cat("Bootstrap SE of phi:", bootstrap_se, "\n")

# Compare theoretical and bootstrap distributions
hist(bootstrap_phi, breaks=20, main="Bootstrap Distribution of phi", xlab="phi")
abline(v=phi_hat, col="red", lwd=2)
abline(v=phi_hat + c(-1.96, 1.96) * theoretical_se, col="blue", lty=2)
abline(v=phi_hat + c(-1.96, 1.96) * bootstrap_se, col="green", lty=2)
legend("topright", legend=c("Estimated phi", "Theoretical 95% CI", "Bootstrap 95% CI"), 
       col=c("red", "blue", "green"), lty=c(1, 2, 2), lwd=c(2, 1, 1))



#Exercise 10.8
data(co2)
month.=season(co2)
trend=time(co2)
model=lm(co2~month.+trend)
summary(model)


acf(residuals(model))


#Exercise 10.9 
win.graph(width=3.25,height=2.5,pointsize=8)
data(airpass)
plot(airpass, type='o',ylab='Air Passengers')

plot(log(airpass), type='o',ylab='Log(Air Passengers)')

win.graph(width=6.5,height=3,pointsize=8)
plot(diff(log(airpass)),type='o',ylab='Difference of Log(Air Passengers)')

plot(diff(log(airpass)),type='l',ylab='Difference of Log(Air Passengers)')
points(diff(log(airpass)),x=time(diff(log(airpass))), pch=as.vector(season(diff(log(airpass)))


plot(diff(diff(log(airpass)),lag=12),type='l',ylab='First & Seasonal Differences of Log(AirPass)')

points(diff(diff(log(airpass)),lag=12),x=time(diff(diff(log(airpass)),lag=12)),pch=as.vector(season(diff(diff(log(airpass)),lag=12)))

       
       
acf(as.vector(diff(diff(log(airpass)),lag=12)),ci.type='ma',
           main='First & Seasonal Differences of Log(AirPass)')


model=arima(log(airpass),order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
summary(model)

win.graph(width=6.5,height=6)
tsdiag(model)

shapiro.test(residuals(model))

win.graph(width=6.5,height=3,pointsize=8)
plot(model,n1=c(1969,1),n.ahead=24,pch=19,ylab='Log(Air Passengers)')

plot(model,n1=c(1969,1),n.ahead=24,pch=19,ylab='Air Passengers',transform=exp)
