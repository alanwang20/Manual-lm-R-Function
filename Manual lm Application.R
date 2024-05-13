# Function
library(tidyverse)
reg_fctn = function(x,y){
  n = length(y) 
  p = ncol(x) 
  
  # calculate regression coefficents(b)
  b = solve(t(x) %*% x) %*% t(x) %*% y
  
  # calculate error vector (ehat)
  ehat = y - x %*% b
  
  # calculate sum of squared residuals(rss)
  rss = sum(ehat^2)
  
  # calculate variance of errors estimate/sigma hat squared (var_e)
  var_e = rss / (n - p)
  
  # calculate variance of coefficients (var_b)
  var_b = diag(solve(t(x) %*% x))
  
  # calculate standard error estimates
  se = sqrt(var_e* var_b)
  
  # calculate t statistic
  t = b/se
  
  # calculate f statistic
  df_bm = n-p
  df_sm = n
  rss_sm <- sum(y^2) # since bm is Y~X-1 ; lm is Y~0. we account for any intercepts in design matrix
  f_stat = (rss_sm-rss)/(df_sm-df_bm)/(rss/df_bm)
  
  
  list(regression_coefficients = b, sigmahat_squared =  var_e, std_errs =  se, t_stats = t, fstat = f_stat, residuals = ehat)
}
# Import Data
sp_df = read_csv("Student_Performance.csv")
sp_df$`Extracurricular Activities` <- ifelse(sp_df$`Extracurricular Activities` == "Yes", 1, 0)

# Run model without intercept
x <- model.matrix(~ `Hours Studied` + `Previous Scores` + `Extracurricular Activities` + `Sleep Hours` + `Sample Question Papers Practiced` - 1, data = sp_df)
y <- sp_df$`Performance Index`
m = reg_fctn(x,y)

# Prove E[e] = 0 
mean(m$residuals)

# Check Constant Variance
residuals_df <- data.frame(Residuals = m$residuals, Observation = seq_along(m$residuals))
ggplot(residuals_df, aes(x = Observation, y = Residuals)) +
  geom_point() +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Plot of Residuals", x = "Observation Index", y = "Residuals") +
  theme_minimal()

# Check Normally Distributed Errors
hist(m$residuals, main = "Histogram of Residuals", xlab = "Residuals", col = "grey", border = "black")
mean_residuals <- mean(m$residuals)
abline(v = mean_residuals, col = "red", lwd = 2)


