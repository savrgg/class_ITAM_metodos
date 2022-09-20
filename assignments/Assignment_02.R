library(tidyverse)
library(tidymodels)


# Ejercicio 1 -------------------------------------------------------------
data <- 
  data.frame(
    x = c(39,43,21,64,57,47,28,75,34,52),
    y = c(65,78,52,82,92,89,73,98,56,75)
  )

# definir 
n = nrow(data)
ybar = mean(data$y)
xbar = mean(data$x)
sxy = cov(data$x, data$y)
sx2 = var(data$x)
sy2 = var(data$y)

lm_fit <- 
  linear_reg() %>% 
  fit(y ~ x, data = data)
tidy(lm_fit)  
data.frame(glance(lm_fit))

# 1a) 
B1 <- sxy/sx2
B0 <- ybar - B1*xbar
# \hat{beta_0} = 40.78416
# \hat{beta_1} = 0.7655618

# 1b) 
rxy = cor(data$x, data$y)

# 1c) 
yhat <- predict(lm_fit, new_data = data %>% select(x))
TSS <- sum((data$y-ybar)**2)
ESS <- sum((yhat-ybar)**2)
RSS <- sum((data$y-yhat)**2)
R2 <- ESS/TSS
adjR2 <- 1- ((1-R2)*(n-1))/(n-1-1)
sigmahat <- sqrt(RSS/(n-2))
sigma_B0 <- sqrt((sigmahat^2)*(1/n + (xbar*xbar)/(sx2*(n-1))))
sigma_B1 <- sqrt((sigmahat^2)/(sx2*(n-1)))

# PH para Beta0
T_B0 = (B0-0)/sigma_B0
2*(1-pt(T_B0, df = n-2))
# PH para Beta0
T_B1 = (B1-0)/sigma_B1
2*(1-pt(T_B1, df = n-2))

# T_B0 = 4.794266
# valor-p B0 = 0.00136551

# T_B1 =  4.375015
# valor-p B1 =  0.002364532

# 1d) 
T_corr = (rxy*sqrt(n-2))/sqrt(1-rxy**2)
2*(1-pt(T_corr, df = n-2))
# T = 4.375015
# valorp = 0.002364532

# 1e)
yhat_50 = B0+B1*50
quantil_t = qt(.975, df = n-2)
lim_inf = yhat_50 - quantil_t*sigmahat*sqrt(1/n + ((50-xbar)^2)/(sx2*(n-1)))
lim_sup = yhat_50 + quantil_t*sigmahat*sqrt(1/n + ((50-xbar)^2)/(sx2*(n-1)))
# IC= (72.51334, 85.61115)

# Ejercicio 2 -------------------------------------------------------------

data <- 
  data.frame(
    x = c(73,71,75,72,72,75,67,69,71,69),
    y = c(185,175,200,210,190,195,150,170,180,175)
  )

# definir 
n = nrow(data)
ybar = mean(data$y)
xbar = mean(data$x)
sxy = cov(data$x, data$y)
sx2 = var(data$x)
sy2 = var(data$y)


lm_fit <- 
  linear_reg() %>% 
  fit(y ~ x, data = data)
tidy(lm_fit)  
data.frame(glance(lm_fit))


# 2a) 
rxy = cor(data$x, data$y)
T_corr = (rxy*sqrt(n-2))/sqrt(1-rxy**2)
2*(1-pt(T_corr, df = n-2))
# rxy = 0.8261048
# T = 4.146421
# valorp = 0.0032

# Ejercicio 3 -------------------------------------------------------------

data <- 
  data.frame(
    x = c(-2,-1,0,1,2),
    y = c(0,0,1,1,3)
  )

# definir 
n = nrow(data)
ybar = mean(data$y)
xbar = mean(data$x)
sxy = cov(data$x, data$y)
sx2 = var(data$x)
sy2 = var(data$y)

lm_fit <- 
  linear_reg() %>% 
  fit(y ~ x, data = data)
tidy(lm_fit)  
data.frame(glance(lm_fit))

# 3a) 
B1 <- sxy/sx2
B0 <- ybar - B1*xbar
# \hat{beta_0} = 1
# \hat{beta_1} = 0.7

# 3b) 
yhat <- predict(lm_fit, new_data = data %>% select(x))
TSS <- sum((data$y-ybar)**2)
ESS <- sum((yhat-ybar)**2)
RSS <- sum((data$y-yhat)**2)
R2 <- ESS/TSS
adjR2 <- 1- ((1-R2)*(n-1))/(n-1-1)
sigmahat <- sqrt(RSS/(n-2))
sigma_B0 <- sqrt((sigmahat^2)*(1/n + (xbar*xbar)/(sx2*(n-1))))
sigma_B1 <- sqrt((sigmahat^2)/(sx2*(n-1)))

yhat_1 = B0+B1*1
quantil_t = qt(.95, df = n-2)
lim_inf = yhat_1 - quantil_t*sigmahat*sqrt(1/n + ((1-xbar)^2)/(sx2*(n-1)))
lim_sup = yhat_1 + quantil_t*sigmahat*sqrt(1/n + ((1-xbar)^2)/(sx2*(n-1)))
# (0.9195, 2.4805)

# 3c), 3d)
# PH para Beta0
T_B0 = (B0-0)/sigma_B0
2*(1-pt(T_B0, df = n-2))
# PH para Beta0
T_B1 = (B1-0)/sigma_B1
2*(1-pt(T_B1, df = n-2))

# T_B0 = 3.692745
# valor-p B0 = 0.03445085

# T_B1 =  3.655631
# valor-p B1 =  0.03535285

# 3e) 
yhat_2 = B0+B1*2
quantil_t = qt(.975, df = n-2)
lim_inf = yhat_2 - quantil_t*sigmahat*sqrt(1+ 1/n + ((2-xbar)^2)/(sx2*(n-1)))
lim_sup = yhat_2 + quantil_t*sigmahat*sqrt(1+ 1/n + ((2-xbar)^2)/(sx2*(n-1)))

# IC= (72.51334, 85.61115)

# ejercicio 4 -------------------------------------------------------------

data <- 
  data.frame(
    x = c(1.21,1.29,1.37,1.46,1.62,1.79),
    y = c(1.302,1.231,1.061,1.040,0.803, 0.711)
  )

# definir 
n = nrow(data)
ybar = mean(data$y)
xbar = mean(data$x)
sxy = cov(data$x, data$y)
sx2 = var(data$x)
sy2 = var(data$y)

lm_fit <- 
  linear_reg() %>% 
  fit(y ~ x, data = data)
tidy(lm_fit)  
data.frame(glance(lm_fit))

# 4a) 
B1 <- sxy/sx2
B0 <- ybar - B1*xbar
# \hat{beta_0} = 1
# \hat{beta_1} = 0.7

# 4b) 
yhat <- predict(lm_fit, new_data = data %>% select(x))
TSS <- sum((data$y-ybar)**2)
ESS <- sum((yhat-ybar)**2)
RSS <- sum((data$y-yhat)**2)
R2 <- ESS/TSS
adjR2 <- 1- ((1-R2)*(n-1))/(n-1-1)
sigmahat <- sqrt(RSS/(n-2))
sigma_B0 <- sqrt((sigmahat^2)*(1/n + (xbar*xbar)/(sx2*(n-1))))
sigma_B1 <- sqrt((sigmahat^2)/(sx2*(n-1)))

# PH para Beta0
T_B0 = (B0-0)/sigma_B0
2*(1-pt(T_B0, df = n-2))
# PH para Beta0
T_B1 = (B1-0)/sigma_B1
2*(pt(T_B1, df = n-2))

# T_B0 = 18.28344
# valor-p B0 = 5.263896e-05

# T_B1 =  -11.0672
# valor-p B1 =  0.0003790737

# 4c)
yhat_15 <- B0+B1*1.5
quantil_t = qt(.95, df = n-2)
lim_inf = yhat_15 - quantil_t*sigmahat*sqrt(1/n + ((1.5-xbar)^2)/(sx2*(n-1)))
lim_sup = yhat_15 + quantil_t*sigmahat*sqrt(1/n + ((1.5-xbar)^2)/(sx2*(n-1)))
# (0.937918, 1.020035)



