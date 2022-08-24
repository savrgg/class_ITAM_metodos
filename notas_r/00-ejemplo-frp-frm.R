library(datarium)
library(tidymodels)  # package that imports useful packages for modeling
library(readr)       # for importing data
library(dotwhisker)  # visualize regression results

data("marketing", package = "datarium")

marketing <- 
  marketing %>% 
  gather(company, investment, -sales) 

marketing %>% View
  
# 02) exploratory data analysis -------------------------------------------

marketing %>% 
  ggplot(aes(x = investment, y = sales, group = company, col = company)) + 
  geom_point() + 
  geom_smooth(method = lm, se = FALSE) +
  facet_wrap(~company, scales = "free_x")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(title = "Relación entre el monto invertido y las ventas",
       subtitle = "Se realizan campañas en tres compañias: youtube, facebook, newspaper",
       caption = "Información recopilada del paquete datarium",
       color = "Compañia", 
       x = "Inversión (miles)",
       y = "Venta (millones")+
  scale_color_viridis_d(option = "plasma", end = .7)

marketing_youtube <- marketing %>% filter(company == "youtube")
# 1 - entrenar modelo
lm_fit <- 
  linear_reg() %>% 
  fit(sales ~ investment, data = marketing_youtube)

# 2 - visualizar y graficar 
tidy(lm_fit)

# 3 - graficar predicciones
new_points <- expand.grid(investment = 0:350)
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
ggplot(plot_data, aes(x = investment)) + 
  geom_point(data = marketing_youtube, aes(x = investment, y = sales))+
  geom_point(aes(y = .pred), alpha = 0.2) + 
  geom_errorbar(aes(ymin = .pred_lower, 
                    ymax = .pred_upper),
                width = .2, alpha = 0.2) + 
  geom_smooth(data = marketing_youtube,aes(x = investment, y = sales), method = "lm")+
  labs(y = "Venta", x = "Inversión") +
  theme_minimal()

# 5 - ¿cómo se ven los coeficientes con las fórmulas?
b1 <- cov(marketing_youtube$sales,marketing_youtube$investment)/var(marketing_youtube$investment)
b0 <- mean(marketing_youtube$sales)-b1*mean(marketing_youtube$investment)


# 6 - ¿Qué pasa si sacamos de una muestra?
  
marketing_youtube_sample = marketing_youtube[sample(1:nrow(marketing_youtube), size = 100),]

ggplot(plot_data, aes(x = investment)) + 
  geom_point(data = marketing_youtube, aes(x = investment, y = sales), alpha = 0.2)+
  geom_point(data = marketing_youtube_sample, aes(x = investment, y = sales), alpha = 0.8, color = "firebrick4")+
  geom_smooth(data = marketing_youtube, aes(x = investment, y = sales), method = "lm", alpha = 0.1)+
  geom_smooth(data = marketing_youtube_sample,aes(x = investment, y = sales), method = "lm", color = "firebrick4")+
  geom_vline(xintercept = mean(marketing_youtube_sample$investment), linetype = 2, color = "firebrick4")+
  geom_hline(yintercept = mean(marketing_youtube_sample$sales), linetype = 2, color = "firebrick4")+
  labs(y = "Venta", x = "Inversión") +
  theme_minimal()



