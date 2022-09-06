
# 0) load libraries -------------------------------------------------------

library(tidyverse)
library(tidymodels)

x <- c(-2, -1, 0, 1, 2)
y <- c(0, 0, 1, 1, 3)

d <- data.frame(x = x, y =y)

d %>%
  ggplot(aes(x = x, y = y))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  labs(title = "Ejemplo regressión lineal")

# 1) fit linear regression ------------------------------------------------

lm_fit <- 
  linear_reg() %>% 
  fit(y ~ x, data = d)

# 2) Incisos de clase -----------------------------------------------------
# 1) 
xbar = mean(d$x)
ybar = mean(d$y)
sx2 = var(d$x)
sy2 = var(d$y)
sxy = cov(d$x, d$y)

# 2) B0, B1
B1 <- sxy/sx2
B0 <- ybar - B1*xbar

# 3) 
yhat <- predict(lm_fit, new_data = d %>% select(x))
TSS <- sum((d$y-ybar)**2)
ESS <- sum((yhat-ybar)**2)
RSS <- sum((d$y-yhat)**2)
R2 <- ESS/TSS
adjR2 <- 1- ((1-R2)*(5-1))/(5-1-1)

# 4) 
sigma2hat <- RSS/(5-2)
sigmahat <- sqrt(sigma2hat)

sigma2_B0 <- sigma2hat*(1/5 + 0/(sx2*(5-1)))
sigma2_B1 <- sigma2hat/(sx2*(5-1))

sigma_B0 <- sqrt(sigma2_B0)
sigma_B1 <- sqrt(sigma2_B1)

# 5) 
T_B1 = (B1-0)/sqrt(sigma2_B1)
T_B0 = (B0-0)/sqrt(sigma2_B0)

2*(1-pt(T_B1, df = 5-2))
2*(1-pt(T_B0, df = 5-2))

# 6) 
rxy = sxy/(sqrt(sx2)*sqrt(sy2))
T_corr = (rxy*sqrt(5-2))/sqrt(1-rxy**2)
2*(1-pt(T_corr, df = 5-2))

# 7)
# recta pasa por la media
d %>%
  ggplot(aes(x = x, y = y))+
  geom_point()+
  geom_smooth(method = "lm")+
  theme_minimal()+
  geom_point(x=xbar, y = ybar, color = "red")+
  labs(title = "Ejemplo regressión lineal")

# suma de residuos es 0
sum(d$y-yhat)
# residuos no están correlacionados con x
r_rx = cor(d$x, d$y-yhat)
# residuos no están correlacionados con yhat
r_ryhat = cor(yhat, d$y-yhat)

# 8) 
Fest = (ESS/1)/(RSS/3)
pf(Fest, df1 = 1, df2 = 3, lower.tail = F)
# De manera resumida ...
  tidy(lm_fit)  
  data.frame(glance(lm_fit))


