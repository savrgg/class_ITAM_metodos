library(tidyverse)
library(tidymodels)
library(readr)

house_rent <- 
  read_csv("notas_r/house_rent.csv") %>% 
  set_names(tolower(names(.))) %>% 
  select(rent, city) %>% 
  filter(city %in% c("Kolkata", "Mumbai"))

house_rent %>% 
  ggplot(aes(x = city, y = rent))+
  geom_boxplot()+
  theme_minimal()+
  ylim(c(0, 100000))

# preprocess
model_recipe <- 
  house_rent %>% 
  recipe(rent ~ city) %>% 
  step_dummy(city)

# model definition
lm_fit <- 
  linear_reg() 

# workflow
model_workflow <- 
  workflow() %>% 
  add_model(lm_fit) %>% 
  add_recipe(model_recipe) %>% 
  fit(data = house_rent)

tidy(model_workflow)
b1 <- cov(xx$rent, xx$city_Mumbai)/var(xx$city_Mumbai)
b0 <- mean(xx$rent)-b1*mean(xx$city_Mumbai)

# interpretacion 
ciudades <- 
  data.frame(city = house_rent$city %>% unique)

predicciones <- 
  predict(model_workflow, new_data = ciudades)

bind_cols(
  ciudades, 
  predicciones
) %>% arrange(city)

house_rent %>% 
  group_by(city) %>% 
  summarise(rent = mean(rent))

#xx <- model_recipe %>% prep() %>% bake(house_rent) %>% data.frame()



# preprocess
model_recipe <- 
  house_rent %>% 
  recipe(rent ~ city) %>% 
  step_dummy(city)

# model definition
lm_fit <- 
  linear_reg() 

# workflow
model_workflow <- 
  workflow() %>% 
  add_model(lm_fit) %>% 
  add_recipe(model_recipe) %>% 
  fit(data = house_rent)

# tidy
tidy(model_workflow)

# predict
predicciones <- 
  predict(model_workflow, new_data = ciudades)

