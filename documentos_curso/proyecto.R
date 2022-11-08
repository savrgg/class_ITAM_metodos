library(readr)
library(tidyverse)
set.seed(119718)
data <- read_csv("documentos_curso/project_data/train.csv")
train <- sample(1:nrow(data), size = nrow(data)*.7, replace = F)

data[train,] %>% head()
data[-train,] %>% head()

data[train,] %>% select(-Id) #%>% write_csv("documentos_curso/project_data/train.csv")
data[-train,] %>% select(-SalePrice) #%>% write_csv("documentos_curso/project_data/test.csv")
data[-train,] %>% select(Id, SalePrice) %>% mutate(Id = as.character(Id))# %>% write_csv("documentos_curso/project_data/sol_file.csv")
data[-train,] %>% 
  select(Id, SalePrice) %>% mutate(t = SalePrice-mean(SalePrice)) %>% 
  mutate(Id = as.character(Id),SalePrice = mean(SalePrice))# %>% write_csv("documentos_curso/project_data/sample_submission.csv")