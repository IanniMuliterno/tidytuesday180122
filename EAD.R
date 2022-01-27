# usethis::use_git()
# usethis::use_github()

library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(lubridate)
library(InformationValue)

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
#########################################################################
# separando ingredientes
model_dt <- chocolate %>%
  mutate(binary_rating = as.character(ifelse(rating < 3,0,1))) %>%
  mutate(qtd_ingredient = str_extract(ingredients,'[0-9]'))

max(str_count(chocolate[!is.na(chocolate$ingredients),]$ingredients,','))


ingred_aux <- str_remove(chocolate$ingredients,'[0-9]')
ingred_aux <- str_squish(str_remove(ingred_aux,'-'))
ingred_aux <- str_split(ingred_aux,',')

#ingred_aux %>% unlist()

n.obs <- sapply(ingred_aux, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(ingred_aux, "[", i = seq.max))

dt_ing <- mat %>%
  as.data.frame() %>%
  select(ing1 = 1, ing2 = 2, ing3 = 3,
         ing4 = 4, ing5 = 5, ing6 = 6)

model_dt <- model_dt %>%
  bind_cols(dt_ing)
#########################################################################
# separando sabores
max(str_count(chocolate[!is.na(chocolate$most_memorable_characteristics),]$most_memorable_characteristics,','))

memorable_aux <- str_squish(str_remove(model_dt$most_memorable_characteristics,'-'))
memorable_aux <- str_split(memorable_aux,',')


n.obs <- sapply(memorable_aux, length)
seq.max <- seq_len(max(n.obs))
mat <- t(sapply(memorable_aux, "[", i = seq.max))

dt_memorable <- mat %>%
  as.data.frame() %>%
  select(memor1 = 1, memor2 = 2, memor3 = 3,
         memor4 = 4, memor5 = 5)


model_dt <- model_dt %>%
  bind_cols(dt_memorable)

names(model_dt)

model_dt2 <- model_dt %>%
  select(2:5,7,11:23)

arvore <- rpart(binary_rating ~.,data = model_dt2,
                control =rpart.control(minsplit = 20,minbucket=20, cp=0.05))

rpart.plot(arvore)

modelo <- glm(binary_rating ~ .,data =model_dt2)
summary(modelo)

##############################################################2

variaveis_cat <- names(model_dt2)[-6]


aux_iv <- model_dt2

lista_saida <- list()


for(cats_index in variaveis_cat){

  i <- which(variaveis_cat == cats_index)

  aux <- aux_iv %>%
    mutate(binary_rating2 = as.numeric(binary_rating)) %>%
    select(variavel = cats_index,binary_rating2)

  lista_saida[[i]] <- c(cats_index,IV(as.factor(aux$variavel), aux$binary_rating2))
}


tab_iv <- data.frame(matrix(unlist(lista_saida), nrow=length(lista_saida), byrow=TRUE)) %>%
  select(var = 1,IV = 2)%>%
  mutate(IV = round(as.numeric(IV),4)) %>%
  arrange(desc(IV))



tab_iv %>%
  mutate(var = factor(var,rev(tab_iv$var))) %>%
  ggplot(aes(y = var, x = IV)) +
  geom_bar(stat = 'identity')

##########################################

binary_rating2 = as.numeric(model_dt2$binary_rating)

aux_woe <- WOETable(X= as.factor(aux_iv %>% pull(variaveis_cat[1])), Y = binary_rating2)
aux_woe <- aux_woe[0,]

for(cats_index in variaveis_cat){

  aux_woe <- aux_woe %>%
    bind_rows(
      WOETable(X= as.factor(aux_iv %>% pull(cats_index)), Y = binary_rating2) %>%
        mutate(variavel = cats_index)

    )



}


aux_woe %>%
  filter(variavel %in% tab_iv$var[1:6]) %>%
  # ggplot(aes(x = fct_reorder(CAT,WOE), y = WOE)) +
  ggplot(aes(x = CAT, y = WOE)) +
  geom_bar(stat = 'identity')+
  facet_wrap(~variavel,scales = 'free') +
  ggtitle("WOE das 6 variáveis com maior IV*")+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  labs(caption = "*Churn: sim = 0\n
                         não = 1")

