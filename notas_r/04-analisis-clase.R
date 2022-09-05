x <- c(-2,-1,0,1,2)
y <- c(0,0,1,1,3)
df <- data.frame(x = x, y = y)
n <- 5
# inciso 1 ======
# media de x
mean(x)
# media de y
mean(y)
# varianza de x
var(x)
(sum(x*x)-n*mean(x)**2)/(n-1)
# varianza de y
var(y)
(sum(y*y)-n*mean(y)**2)/(n-1)
# covarianza x,y
cov(x,y)
(sum(y*x)-n*mean(y)*mean(x))/(n-1)

# inciso 2 ======
lm_fit <- 
  linear_reg() %>% 
  fit(y ~ x, data = df)

y_hat <- predict(lm_fit, new_data = df %>% select(x))
y <- df$y
y_mean <- mean(df$y)

TSS = sum((y - y_mean)**2)
ESS = sum((y_hat - y_mean)**2)
RSS = sum((y - y_hat)**2)

R2 = ESS/TSS
adjR2 = 1-((1-0.8167)*(n-1)/(n-1-1))
tidy(lm_fit)
glance(lm_fit)