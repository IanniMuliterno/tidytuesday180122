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
