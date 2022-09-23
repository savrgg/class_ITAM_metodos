# Normalidad
library(tidyverse)




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

