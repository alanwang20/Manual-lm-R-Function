n = 50
X0 = matrix(rep(1, 50), ncol=1)
X1 = rnorm(n)
X2 = rnorm(n)
X3 = rnorm(n)
e = rnorm(n)
Y = X0 + X1+X2  + X3 +e
X = cbind(1,X1,X2,X3)
n = length(Y)
p = ncol(X)

reg_fctn = function(x,y){
  n = length(y) # there are 
  p = ncol(x) # there are 3 variables
  # calculate regression coefficents(b)
  b = solve(t(x) %*% x) %*% t(x) %*% y
  
  # calculate error vector (e)
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
  df_sm = n-1
  rss_sm = sum((y - mean(y))^2)
  f_stat = (rss_sm-rss)/(df_sm-df_bm)/(rss/df_bm)
  
  list(regression_coefficients = b, sigmahat_squared =  var_e, std_errs =  se, t_stats = t, fstat = f_stat)
}

reg_fctn(X,Y)
summary(lm(Y~X1 +X2+ X3))



