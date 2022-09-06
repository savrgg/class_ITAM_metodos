
# 01) load libraries and data ---------------------------------------------



# urchins[c(1,72,68),] %>% set_names("Régimen alimenticio", "Volumen inicial", "Ancho de sutura") %>% flextable::flextable()

# 02) exploratory data analysis -------------------------------------------







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



