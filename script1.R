# PACOTES ----
if (!require(pacman)){
  install.packages("pacman")} else{
library(pacman)}

pacman::p_load(tidyverse,  janitor, summarytools, kableExtra)
# pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools)
# pacman::p_load( kableExtra, moments, ggpubr, formattable, gridExtra)
# 
# pacman::p_load(glue, corrplot, sessioninfo, readxl, writexl, ggthemes)
# 
# pacman::p_load(patchwork, qqplotr, plotly, lmtest, olsrr, gglm,
#                tidymodels)


pacman::p_load(gitcreds, usethis)

# https://curso-r.githud.io/zen-do-r/git-githud.html
gitcreds::gitcreds_set()
usethis::use_git()
usethis::use_github()


# USANDO R BASE ----
## Q1 ----
{
### DADOS 1 ----

dados_brutos1 <- read.csv("dados/caschool.csv")

dplyr::glimpse(dados_brutos1)

dados1 <- dados_brutos1|>
  janitor::clean_names()

### item a ----
dados1|>
  summary()

### item b ----
plot(dados1$math_scr, dados1$str)

### item c ----

dp = function(x){
  n = length(x)
  m = sum(x)/n
  desvio = (x - m)^2
  var = sum(desvio)/(n-1)
  dp = sqrt(var)
  
  return(dp)
}

dp(dados1$math_scr)
sqrt(var(dados1$math_scr)) # Conferência

dp(dados1$str)
sqrt(var(dados1$str)) # Conferência

### item d ----

# x=c(5, 10, 15, 20, 25, 30, 35, 40, 45, 50)
# y=c(65, 120, 210, 260, 380, 450, 510, 555, 615, 660)
# 
# (n = length(x))
# (soma_x = sum(x))
# (soma_x2 = sum(x^2))
# (s_xx = soma_x2-(((soma_x)^2)/n))
# 
# (soma_y = sum(y))
# (soma_y2 = sum(y^2))
# (s_yy = soma_y2-(((soma_y)^2)/n))
# 
# (soma_xy = sum(x*y))
# (s_xy = soma_xy-((soma_x*soma_y)/n))
# 
# (r = s_xy/(sqrt(s_xx*s_yy)))

corr_pearson = function(x, y){
  n = length(x)
  soma_x = sum(x)
  soma_x2 = sum(x^2)
  s_xx = soma_x2-(((soma_x)^2)/n)
  
  soma_y = sum(y)
  soma_y2 = sum(y^2)
  s_yy = soma_y2-(((soma_y)^2)/n)
  
  soma_xy = sum(x*y)
  s_xy = soma_xy-((soma_x*soma_y)/n)
  
  r = s_xy/(sqrt(s_xx*s_yy))
  
  return(r)
}

corr_pearson(dados1$math_scr, dados1$str)

cor(dados1$math_scr, dados1$str)
}
## Q2 ----
{
### DADOS 2 ----
dados_brutos2 <- read.table('dados/pima.ascii', sep = "", head=T)

dplyr::glimpse(dados_brutos2)

### item a ----
summary(dados_brutos2)

### item b ----

# pima$diastolic[pima$diastolic == 0] = NA
dados_brutos2$diastolic[dados_brutos2$diastolic == 0] = NA


### item c ----



### item d ----

}

# USANDO PKT ----

dados1|>
  # filter(sex == "f")|>select(sex)|>count()
# rename("Largura Crânio" = skullw, "Comprimento Total" = totlngth)|>
  summarytools::descr(
    stats = c("min", "q1", "med", "mean","q3", "max",  "sd", "cv"),
    # round.digits = 3,
    justify = "c",
    style = "grid", #' rmarkdown',
    transpose = T
  )|>
  kbl(
    caption = "Tabela 1: Medidas Resumo para o sexo feminino.",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = T,
    col.names =
      c("Min", "Q1", "Med", "Média", "Q3", "Max", "Desvio Padrão", "CV")
  )|>
  kable_material(c("striped", "hover", "condensed"))|>
  # kadle_styling(
  #   # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
  #   dootstrap_options = c("striped", "hover"),
  #   full_width = F,
  #   fixed_thead = T # Fixa o cadeçalho ao rolar a tadela.
  # ) %>%
  # footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |>
  kable_material()
# add_header_adove(c("Características", "Medidas de Tendência Central e Variadilidade" = 8))

