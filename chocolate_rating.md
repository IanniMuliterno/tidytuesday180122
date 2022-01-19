Untitled
================

## GitHub Documents

Olá, esta é minha primeira vez participando de um tidytuesday, a ideia é
ver o que consigo fazer dedicando 2h à analise de uma determinada base.

### Carregando dados

``` r
library(tidyverse)
```

    ## -- Attaching packages --------------------------------------- tidyverse 1.3.1 --

    ## v ggplot2 3.3.5     v purrr   0.3.4
    ## v tibble  3.1.6     v dplyr   1.0.7
    ## v tidyr   1.1.4     v stringr 1.4.0
    ## v readr   2.0.1     v forcats 0.5.1

    ## -- Conflicts ------------------------------------------ tidyverse_conflicts() --
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(ggplot2)

chocolate <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-01-18/chocolate.csv')
```

    ## Rows: 2530 Columns: 10

    ## -- Column specification --------------------------------------------------------
    ## Delimiter: ","
    ## chr (7): company_manufacturer, company_location, country_of_bean_origin, spe...
    ## dbl (3): ref, review_date, rating

    ## 
    ## i Use `spec()` to retrieve the full column specification for this data.
    ## i Specify the column types or set `show_col_types = FALSE` to quiet this message.

Nota de alguns pontos observados por especialistas em chocolate

-   tempo de produção
-   qualidade dos ingredientes
    -   muito açucar
    -   derrete na boca
-   shape

``` r
chocolate %>%
  group_by(company_manufacturer,company_location) %>%
  group_by(company_location) %>%
  count() %>%
  arrange(desc(n))
```

    ## # A tibble: 67 x 2
    ## # Groups:   company_location [67]
    ##    company_location     n
    ##    <chr>            <int>
    ##  1 U.S.A.            1136
    ##  2 Canada             177
    ##  3 France             176
    ##  4 U.K.               133
    ##  5 Italy               78
    ##  6 Belgium             63
    ##  7 Ecuador             58
    ##  8 Australia           53
    ##  9 Switzerland         44
    ## 10 Germany             42
    ## # ... with 57 more rows

## No proximo round comece aqui

-   quebrar por virgular assim podemos ver a caracteristica marcantes
    mais associada com altos valores (regressao)

## Possibilidade

Nos primeiros 15 minutos avaliei as possibilidades - é possível
verificar o impacto do tempo e verificar a evolução das notas em relação
a percentual de cacau para entender se e como o paladar dos consumidores
mudou ao longo do tempo

-   é possivel verificar se o fato do produtor usar materia prima local
    ou importada parece influenciar na nota

-   utilizar pacote da aula de redes neurais para refazer categoricas de
    maneira ordenada com a variavel resposta

-   50 minutos investidos, foi notado que

-   apenas ingredientes tem NA fora isso não existem campos vazios

-   existem alguns outliers como country\_manufacturer com concentracao
    em EUA

-   chocolate costuma ter \[60%-80%\] cacau

-   quebrar por virgular assim podemos ver a caracteristica marcantes
    mais associada com altos valores (regressao)

-   posso associar informacoes censitarias, idh ou geograficas como
    continente etc aos paises

-   para entender o impacto disso na opnião das pessoas sobre o
    chocolate ou até mesmo questões de qualidade ou metodo de fabricacao
    ![](chocolate_rating_files/figure-gfm/pressure-1.png)<!-- -->

Note that the `echo = FALSE` parameter was added to the code chunk to
prevent printing of the R code that generated the plot.
