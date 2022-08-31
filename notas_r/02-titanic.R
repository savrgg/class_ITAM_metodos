library(tidymodels)
library(readr)

datos <- read_csv("~/Repos/class_ITAM_metodos/notas_r/train.csv")

datos_o <- 
  datos %>% 
  mutate(
    Survived_prev = Survived,
    Survived = factor(Survived, labels = c("No Sobrevivió", "Sobrevivió")),
         Age = case_when(
           Age < 3 ~ "[0-03) \n Bebés",
           Age < 10 ~ "[03-10) \n Niños/Niñas",
           Age < 18 ~ "[10-18) \n Adolescentes",
           Age < 30 ~ "[18-30) \n Adultos Jóvenes",
           Age < 45 ~ "[30-45) \n Adultos Media",
           Age < 65 ~ "[45-65) \n Adultos Maduros",
           Age < 120 ~ "[65+) \n Tercera Edad",
           TRUE ~ "No proporcionado"
           ),
         Title = gsub('(.*, )|(\\..*)', '', Name))

var_p <- "Sex"
#var_p <- "Pclass"
#var_p <- "Age"
#var_p <- "Title"


datos_o %>% 
  ggplot(aes(x = factor(!!sym(var_p)), fill = Survived))+
  geom_bar()+
  geom_text(aes(label = after_stat(count)), 
            stat = "count", position = position_stack(), vjust = 1, color = "gray50", size = 3)+
  scale_fill_brewer(palette = "Greens")+
  theme_minimal()+
  theme(legend.position = "bottom")+
  labs(
    x = "",
    y = "Número de sobrevivientes",
    title = "Número de sobrevivientes por género en el Titanic (1912)",
    subtitle = "Número total de pasajeros en base explorada: 891. Porcentaje de Sobrevivencia: 38.4%",
    caption = "Base extraída de: kaggle.com/competitions/titanic",
    fill = ""
  )

datos_o %>% 
  ggplot(aes(x = factor(!!sym(var_p)), fill = Survived))+
  geom_bar(position = "fill", aes(fill = Survived))+
  geom_text(aes(label = scales::percent(after_stat(count/tapply(count, x, sum)[x])), group = Survived), 
            stat = "count", position = position_fill(), vjust = 2, color = "gray50", size = 3)+
  scale_fill_brewer(palette = "Greens")+
  theme_minimal()+
  scale_y_continuous(labels = percent_format())+
  theme(legend.position = "bottom")+
  labs(
    x = "",
    y = "Porcentaje de sobrevivientes",
    title = "Porcentaje de sobrevivientes por género en el Titanic (1912)",
    subtitle = "Número total de pasajeros en base explorada: 891. Porcentaje de Sobrevivencia: 38.4%",
    caption = "Base extraída de: kaggle.com/competitions/titanic",
    fill = ""
  )

datos_o %>% 
  group_by(Sex, Pclass, Age) %>% 
  summarise(num_pasajeros = n(), 
            sobreviven = sum(Survived_prev),
            porc_sobr = sobreviven/num_pasajeros) %>% 
  arrange(desc(porc_sobr)) %>% data.frame()

