# FA 2 -TIME SERIES ANALYSIS
# SAMSON, Justine Aizel D.


# Load required libraries
library(lmtest)
library(nlme)
library(orcutt)

# Load the data
data <- data.frame(
  t = 1:15,
  xt = c(100, 98, 100, 89, 95, 87, 93, 82, 85, 83, 81, 79, 90, 77, 78),
  yt = c(15.93, 16.26, 15.94, 16.81, 15.67, 16.47, 15.66, 16.94, 16.60, 17.16, 17.77, 18.05, 16.78, 18.17, 17.25)
)

# Part a: Fit a simple linear regression model
fit <- lm(yt ~ xt, data = data)
summary(fit)

# Part a: Plot residuals versus time
residuals <- resid(fit)
plot(data$t, residuals, xlab = "Time (t)", ylab = "Residuals", main = "Residuals vs Time")
abline(h = 0, col = "blue")

# Optional: Plot autocorrelation of residuals
acf(residuals, main = "ACF of Residuals")

# Part b: Perform Durbin-Watson test for autocorrelation
dw_test <- dwtest(fit)
print(dw_test)

# Part c: Apply Cochrane-Orcutt procedure (first iteration)
cochrane_fit <- cochrane.orcutt(fit)
summary(cochrane_fit)

# Part d: Perform Durbin-Watson test after Cochrane-Orcutt procedure
dw_test_after <- dwtest(cochrane_fit)
print(dw_test_after)

# Part e: Fit a time series regression model with autocorrelated errors
gls_fit <- gls(yt ~ xt, correlation = corAR1(form = ~t), data = data)
summary(gls_fit)

