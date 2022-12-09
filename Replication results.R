install.packages("mvtnorm")
library("mvtnorm")
install.packages("robustbase")
library("robustbase")
install.packages("MASS")
library("MASS")

# Control Parameters
set.seed(123)
R <- 500
n <- 1000
meanlog <- 0
sdlog <- 1
df <- 2
mu <- rep(0,3)
sgmX <- matrix(c(1,0.5, 0.25, 0.5, 1, 0.5, 0.25, 0.5, 1),3,3)
bt <- c(1, 0.5, 0.25)

# Uncontaminated data
resultsUncontaminated <- replicate(R, {
  # Generate data
  x <- rmvnorm(n, mean = mu, sigma = sgmX)
  LP <- x%*%bt
  y <- rnorm(n, LP, sgmX)
  
  # Ordinary Least Squares (fit, param., var.)
  fit.ols <- lm(y~x)
  coef.ols <- coef(lm(y~x))
  var.ols <- coefficients(summary(fit.ols))[,2]
  tstat.ols <- coefficients(summary(fit.ols))[,3]
  sign.ols <- coefficients(summary(fit.ols))[,4]
  
  # Mallows-type Estimator (fit, param., var.)
  fit.mallowsm_est = glmrob(y~x, family = gaussian(link = identity), method = "Mqle", weights.on.x = "covMcd")
  coef.mallowsm_est <- coef(fit.mallowsm_est)
  var.mallowsm_est <- coefficients(summary(fit.mallowsm_est))[,2]
  tstat.mallowsm_est <- coefficients(summary(fit.mallowsm_est))[,3]
  sign.mallowsm_est <- coefficients(summary(fit.mallowsm_est))[,4]
  
  # Second way of Mallows-type M
  # fit.mallowsm_est <- rlm(y_contam~x_contam, method = "M")
  # coef.mallowsm_est <- coef(fit.mallowsm_est)
  # var.mallowsm_est <- sigma(fit.mallowsm_est)
  
  ## S-estimator
  # fit.s_est <- lmrob.S(x=x_contam, y=y_contam, control=lmrob.control(trace.lev = 0))
  # coef.s_est <- coef(fit.s_est)
  # var.s_est <- fit.s_est$qr(cov)
  
  # MM-estimator 
  fit.mm_est <- lmrob(y~x)
  coef.mm_est <- coefficients(summary(fit.mallowsm_est))[,1]
  var.mallowsm_est <- coefficients(summary(fit.mm_est))[,2]
  tstat.mallowsm_est <- coefficients(summary(fit.mm_est))[,3]
  sign.mallowsm_est <- coefficients(summary(fit.mm_est))[,4]
  
  
  c(coef.ols, var.ols, tstat.ols, sign.ols, coef.mallowsm_est, var.mallowsm_est, tstat.mallowsm_est, sign.mallowsm_est, coef.mm_est, var.mm_est, tstat.mm_est, sign.mm_est)
})
estim_var <- rowMeans(resultsUncontaminated)
rowMeans(results)
par(mfrow = c(1, 4))
boxplot(resultsOutlier[1,], resultsOutlier[17,], resultsOutlier[33,]); abline(h = 0)
boxplot(resultsOutlier[2,], resultsOutlier[18,], resultsOutlier[34,]); abline(h = bt[1])
boxplot(resultsOutlier[3,], resultsOutlier[19,], resultsOutlier[35,]); abline(h = bt[2])
boxplot(resultsOutlier[4,], resultsOutlier[20,], resultsOutlier[36,]); abline(h = bt[3])

# Contaminated data
resultsOutlier <- replicate(R, {
  # Generate data
  x <- rmvnorm(n, mean = mu, sigma = sgmX)
  LP <- x%*%bt
  y <- rnorm(n, LP, sgmX)
  
  # Generate contaminated data
  eps <- rbinom(n, 1, 0.05) 
  y_contam <- (1-eps)*y + eps*0
  x_contam <- (1-eps)*x + eps*matrix(rep(-2, n*3), n, 3) # Either from original data or outlier
  
  # Ordinary Least Squares (fit, param., var.)
  fit.ols <- lm(y_contam~x_contam)
  coef.ols <- coef(lm(y_contam~x_contam))
  var.ols <- coefficients(summary(fit.ols))[,2]
  tstat.ols <- coefficients(summary(fit.ols))[,3]
  sign.ols <- coefficients(summary(fit.ols))[,4]
  
  # Mallows-type Estimator (fit, param., var.)
  fit.mallowsm_est = glmrob(y_contam~x_contam, family = gaussian(link = identity), method = "Mqle", weights.on.x = "covMcd")
  coef.mallowsm_est <- coef(fit.mallowsm_est)
  var.mallowsm_est <- coefficients(summary(fit.mallowsm_est))[,2]
  tstat.mallowsm_est <- coefficients(summary(fit.mallowsm_est))[,3]
  sign.mallowsm_est <- coefficients(summary(fit.mallowsm_est))[,4]
  
  # Second way of Mallows-type M
  # fit.mallowsm_est <- rlm(y_contam~x_contam, method = "M")
  # coef.mallowsm_est <- coef(fit.mallowsm_est)
  # var.mallowsm_est <- sigma(fit.mallowsm_est)
  
  ## S-estimator
  # fit.s_est <- lmrob.S(x=x_contam, y=y_contam, control=lmrob.control(trace.lev = 0))
  # coef.s_est <- coef(fit.s_est)
  # var.s_est <- fit.s_est$qr(cov)
  
  # MM-estimator 
  fit.mm_est <- lmrob(y_contam~x_contam)
  coef.mm_est <- coefficients(summary(fit.mallowsm_est))[,1]
  var.mallowsm_est <- coefficients(summary(fit.mm_est))[,2]
  tstat.mallowsm_est <- coefficients(summary(fit.mm_est))[,3]
  sign.mallowsm_est <- coefficients(summary(fit.mm_est))[,4]
  
  
  c(coef.ols, var.ols, tstat.ols, sign.ols, coef.mallowsm_est, var.mallowsm_est, tstat.mallowsm_est, sign.mallowsm_est, coef.mm_est, var.mm_est, tstat.mm_est, sign.mm_est)
})
estim_var <- rowMeans(resultsOutlier)
rowMeans(results)
par(mfrow = c(1, 4))
boxplot(resultsOutlier[1,], resultsOutlier[17,], resultsOutlier[33,]); abline(h = 0)
boxplot(resultsOutlier[2,], resultsOutlier[18,], resultsOutlier[34,]); abline(h = bt[1])
boxplot(resultsOutlier[3,], resultsOutlier[19,], resultsOutlier[35,]); abline(h = bt[2])
boxplot(resultsOutlier[4,], resultsOutlier[20,], resultsOutlier[36,]); abline(h = bt[3])

# WILCOXON TEST FOR VARIANCES (STANDARD DEVIATION)
# Wilcoxon Test Beta0
OLSvsMallowsVar0 <- p.value(wilcox.test(resultsOutlier[5,], resultsOutlier[21,], paired=FALSE, alternative = "less")) # Wilcoxon-test OLS vs Mallows-M
OLSvsMMVar0 <- wilcox.test(resultsOutlier[5,], resultsOutlier[37,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM

# Wilcoxon Test Beta1
OLSvsMallowsVar1 <- wilcox.test(resultsOutlier[6,], resultsOutlier[22,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs Mallows-M
OLSvsMMVar1 <- wilcox.test(resultsOutlier[6,], resultsOutlier[38,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM

# Wilcoxon Test Beta2
OLSvsMallowsVar2 <- p.value(wilcox.test(resultsOutlier[7,], resultsOutlier[23,], paired=FALSE, alternative = "less")) # Wilcoxon-test OLS vs Mallows-M
OLSvsMMVar2 <- wilcox.test(resultsOutlier[7,], resultsOutlier[39,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM

# Wilcoxon Test Beta3
OLSvsMallowsVar3 <- p.value(wilcox.test(resultsOutlier[8,], resultsOutlier[24,], paired=FALSE, alternative = "less")) # Wilcoxon-test OLS vs Mallows-M
OLSvsMMVar3 <- wilcox.test(resultsOutlier[8,], resultsOutlier[40,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM

# WILCOXON TEST FOR T-STATISTICS
# Wilcoxon Test Beta0
OLSvsMallowsTstat0 <- p.value(wilcox.test(resultsOutlier[9,], resultsOutlier[25,], paired=FALSE, alternative = "less")) # Wilcoxon-test OLS vs Mallows-M
OLSvsMMTstat0 <- wilcox.test(resultsOutlier[9,], resultsOutlier[41,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM

# Wilcoxon Test Beta1
OLSvsMallowsTstat1 <- wilcox.test(resultsOutlier[10,], resultsOutlier[26,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs Mallows-M
OLSvsMMTstat1 <- wilcox.test(resultsOutlier[10,], resultsOutlier[42,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM

# Wilcoxon Test Beta2
OLSvsMallowsTstat2 <- p.value(wilcox.test(resultsOutlier[11,], resultsOutlier[27,], paired=FALSE, alternative = "less")) # Wilcoxon-test OLS vs Mallows-M
OLSvsMMTstat2 <- wilcox.test(resultsOutlier[11,], resultsOutlier[43,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM

# Wilcoxon Test Beta3
OLSvsMallowsTstat3 <- p.value(wilcox.test(resultsOutlier[12,], resultsOutlier[28,], paired=FALSE, alternative = "less")) # Wilcoxon-test OLS vs Mallows-M
OLSvsMMTstat3 <- wilcox.test(resultsOutlier[12,], resultsOutlier[44,], paired=FALSE, alternative = "less") # Wilcoxon-test OLS vs MM









