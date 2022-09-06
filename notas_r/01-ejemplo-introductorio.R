
# 01) load libraries and data ---------------------------------------------

library(tidymodels)  # package that imports useful packages for modeling
library(readr)       # for importing data
library(dotwhisker)  # visualize regression results

# 72 urchins ==
# experimental feeding regime group
urchins <-
  read_csv("notas_r/urchins.csv") %>% 
  setNames(c("food_regime", "initial_volume", "width")) %>% 
  mutate(food_regime = factor(food_regime, levels = c("Initial", "Low", "High")))

# urchins[c(1,72,68),] %>% set_names("Régimen alimenticio", "Volumen inicial", "Ancho de sutura") %>% flextable::flextable()

# 02) exploratory data analysis -------------------------------------------

urchins %>% 
  ggplot(aes(x = initial_volume, y = width, group = food_regime, col = food_regime)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~food_regime)+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Relación entre los distintos niveles de alimentación",
       subtitle = "Total de muestra: 72 erizos bajo tres tratamientos",
       caption = "Información recopilada de Constable, A.J., 1993",
       color = "Régimen alimenticio", 
       x = "Volumen Inicial",
       y = "Ancho de suturas")+
  scale_color_viridis_d(option = "plasma", end = .7)

# experimental feeding regime group (food_regime: either Initial, Low, or High),
# size in milliliters at the start of the experiment (initial_volume)
# suture width at the end of the experiment (width).

# 1 - entrenar modelo
lm_fit <- 
  linear_reg() %>% 
  fit(width ~ initial_volume, data = urchins)

# 2 - visualizar y graficar 
tidy(lm_fit)

# 3 - graficar predicciones
new_points <- expand.grid(initial_volume = 0:50)
mean_pred <- predict(lm_fit, new_data = new_points)
conf_int_pred <- 
  predict(
    object = lm_fit, 
    new_data = new_points, 
    type = "conf_int",
    level = .95
    )

plot_data <- 
  new_points %>% 
  bind_cols(mean_pred) %>% 
  bind_cols(conf_int_pred)

# 4 - graficar datos
ggplot(plot_data, aes(x = initial_volume)) + 
  geom_point(data = urchins, aes(x = initial_volume, y = width))+
  geom_point(aes(y = .pred), alpha = 0.2) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2, alpha = 0.2) + 
  geom_smooth(data = urchins,aes(x = initial_volume, y = width), method = "lm")+
  labs(y = "Ancho de suturas", x = "Volumen Inicial") +
  theme_minimal()

# 5 - ¿cómo se ven los coeficientes con las fórmulas?
b1 <- cov(urchins$width,urchins$initial_volume)/var(urchins$initial_volume)
b0 <- mean(urchins$width)-b1*mean(urchins$initial_volume)



# ¿cómo podemos medir el error? lo veremos más adelante...
# TSS (Total Sum of Squares)
tss = sum((urchins$width-mean(urchins$width))**2)
# RSS (Residual Sum of Squares)
rss = sum((urchins$width-predict(lm_fit, new_data = urchins %>% select(-width)))**2)
# RSE: Residual Standard Error (sigma) TTS
sum_squares = sum((urchins$width-predict(lm_fit, new_data = urchins %>% select(-width)))**2)/(72-2)
rse = round(sqrt(sum_squares),4)
# R2: (TTS-RSS)/TTS
r2 <- (tss-rss)/tss
# adjR2: 
1-((1-r2)*(72-1))/(72-1-1)

glance(lm_fit)
tidy(lm_fit)

sqrt(sum((urchins$width-predict(lm_fit, new_data = urchins %>% select(-width)))**2))/(70)



