# usethis::use_git()
# usethis::use_github()

library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)

#referencia
#https://github.com/rfordatascience/tidytuesday/tree/master/data/2022/2022-01-18
#https://www.learnbymarketing.com/tutorials/rpart-decision-trees-in-r/
chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')

############################################################
## basics

sapply(chocolate, function(x) sum(is.na(x)))
sapply(chocolate, function(x) sum(str_trim(x) == ""))

chocolate %>%
  filter(is.na(ingredients)) %>%
 View()
# em maioria tem 100% cacau ou teor de cacau muito alto

#https://r4ds.had.co.nz/exploratory-data-analysis.html
cat_cols <- chocolate %>%
  select_if(is.character) %>%
  names() %>%
  length()
cat_cols <- 1:cat_cols

cat_check <- list()

# for(i in cat_cols){
#
#   nome <- chocolate %>%
#     select_if(is.character) %>%
#     select(i) %>%
#     names()
#
#   print(chocolate %>%
#     select_if(is.character) %>%
#     select(variavel = i) %>%
#   ggplot(aes(x = variavel)) +
#   geom_bar()+
#     ggtitle(paste0("distrib. de ",nome)))
# }

for(i in cat_cols){


  cat_check[[i]] <-  chocolate %>%
          select_if(is.character) %>%
          select(x = i) %>%
    group_by(x) %>%
    count()
}

cat_check[[1]] %>% View()
cat_check[[2]] %>% View()
cat_check[[3]] %>% View()
cat_check[[4]] %>% View()
cat_check[[5]] %>% View() #chocolate costuma ter [60%-80%] cacau
cat_check[[6]] %>% View()
cat_check[[7]] %>% View() #quebrar por virgular assim podemos ver a caracteristica marcantes mais associada com altos valores (regressao)

# posso associar informacoes censitarias, idh ou geograficas como continente etc aos paises
# para entender o impacto disso na opnião das pessoas sobre o chocolate ou até mesmo questões de qualidade ou metodo de fabricacao

chocolate %>%
  group_by(company_manufacturer,company_location) %>%
  group_by(company_location) %>%
  count() %>%
  arrange(desc(n))

chocolate %>%
  group_by(company_manufacturer,company_location,review_date) %>%
  count() %>%
  ggplot(aes(x = review_date, color = company_location))+
  geom_boxplot()

# alguns chocolates são avaliados em periodos muito diferentes, isso pode influenciar no resultado
# a definição do q é um chocolate pode mudar , por exempllo.

names(chocolate)
n_distinct(chocolate$ref)



arvore <- rpart(rating ~.,data = chocolate,
                control =rpart.control(minsplit = 20,minbucket=20, cp=0.05))
rpart.plot(arvore)

modelo <- lm(rating ~ .,data =chocolate)
summary(modelo)
