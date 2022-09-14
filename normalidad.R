# caja y brazos
data_1 <- 
  data.frame(
    obs = 1:1000, 
    value = rexp(1000, rate = 1/5),
    value_normal = rnorm(1000)
  ) 

data_1 %>% 
  ggplot(aes(x = ))


# histograma del error 


# qqplot
# ejemplo colas pesadas
data_1 <- 
  data.frame(
    obs = 1:1000, 
    value = rexp(1000, rate = 1/5),
    value_normal = rnorm(1000)
  ) 

data_1 %>% 
  ggplot()+
  stat_qq(aes(sample = value) )+
  stat_qq_line(aes(sample = value_normal) )+
  theme_minimal()

data_1 %>% 
  ggplot()+
  geom_density(aes(x = value), color = "blue")+
  geom_density(aes(x = value_normal), color = "red")+
  theme_minimal()





# jarque bera

library(tseries)
library(tidyverse)
library(moments)

data_1 <- 
  data.frame(
    obs = 1:1000, 
    value = rt(1000, df = 5),
    value_normal = rnorm(1000)
  ) 

# a mano
data_1 %>% 
  ggplot()+
  geom_density(aes(x = value), color = "blue")+
  geom_density(aes(x = value_normal), color = "red")+
  theme_minimal()

nrows = nrow(data_1)

sk = (sum((data_1$value-mean(data_1$value))**3)/nrows) / (var(data_1$value)*(nrows-1)/nrows)**(3/2)
k =  (sum((data_1$value-mean(data_1$value))**4)/nrows) / (var(data_1$value)*(nrows-1)/nrows)**(4/2)
JB = nrows*((sk^2)/6 + (k-3)*(k-3)/(24)) 
pchisq(JB, lower.tail = F, df = 2)

sk_normal = sum((data_1$value_normal-mean(data_1$value_normal))**3)/nrows / (var(data_1$value_normal)*(nrows-1)/nrows)**(3/2)
k_normal =  (sum((data_1$value_normal-mean(data_1$value_normal))**4)/nrows) / (var(data_1$value_normal)*(nrows-1)/nrows)**(4/2)
JB_normal = nrows*((sk_normal^2)/6 + (k_normal-3)*(k_normal-3)/(24)) 
pchisq(JB_normal, lower.tail = F, df = 2)

# formula
skewness(data_1$value)
kurtosis(data_1$value)
jarque.bera.test(data_1$value)

skewness(data_1$value_normal)
kurtosis(data_1$value_normal)
jarque.bera.test(data_1$value_normal)

