library(readr)
library(tidyverse)
library(latex2exp)
set.seed(123456)

lista_cu<-c(166418,
188175,
179528,
188108,
168914,
180747,
179503,
181581,
190125,
187932,
119718,
179521,
189236,
189539,
123298,
173229,
170463,
165011,
181062,
183916,
172331,
184068,
170175,
182522,
181338,
185063,
184187,
182022,
191712,
189715,
165641)

data <- 
  read_csv("https://raw.githubusercontent.com/savrgg/class_ITAM_metodos/main/notas_r/datos_parcial1/maths.csv") %>% 
  set_names(tolower(names(.))) %>% 
  select(g3, absences, age, traveltime, studytime, failures, famrel, freetime, goout, dalc, walc, health)

gen_datos <- function(clave_unica){
  sample_data <- sample(1:nrow(data), 200)  
  datos_sample <- data[sample_data, ]
  write_csv(datos_sample, paste0("notas_r/datos_parcial1/maths", clave_unica, ".csv"))
}

textPlot <- function(plotname, string){
  par(mar=c(0,0,0,0))
  pdf(paste0(plotname, ".pdf"))
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, paste(string), cex = 4, col = "black", family="serif", font=2, adj=0.5)
  dev.off()
}

gen_formula <- function(clave_unica){
  sample_column <- sample(data %>% select(-g3) %>% names, 1)
  
  sample_formula <- sample(c("sqrt", "exp", "abs", "cuadrado"), 1)
  if(sample_formula == "sqrt"){
    text <- TeX(paste0('g3 = $\\beta_0$ + $\\beta_1$ \\', sample_formula, '{', sample_column,'}$'))  
  }
  if(sample_formula == "ln"){
    text <- TeX(paste0('g3 = $\\beta_0$ + $\\beta_1$ \\', sample_formula, '(', sample_column,')$'))  
  }
  if(sample_formula == "exp"){
    text <- TeX(paste0('g3 = $\\beta_0$ + $\\beta_1$ \\', sample_formula, '(', sample_column,')$'))  
  }
  if(sample_formula == "abs"){
    text <- TeX(paste0('g3 = $\\beta_0$ + $\\beta_1$ |',sample_column,'|$'))  
  }
  if(sample_formula == "cuadrado"){
    text <- TeX(paste0('g3 = $\\beta_0$ + $\\beta_1 ',sample_column,'^2$'))  
  }
  p1 <- ggplot()+
    annotate("text", x= 0 , y = 0, label = text)+
    theme_void()
  
  ggsave(filename = paste0("notas_r/datos_parcial1/formula", clave_unica, ".png"), plot = p1)
}

map(lista_cu, gen_datos)
map(lista_cu, gen_formula)






