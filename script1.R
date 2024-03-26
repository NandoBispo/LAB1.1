# PACOTES ----
if (!require(pacman)){
  install.packages("pacman")} else{
library(pacman)}

pacman::p_load(tidyverse,  janitor, summarytools, kableExtra, patchwork, plyr)

# pacman::p_load(tidyverse,  janitor, stargazer,  sjmisc, summarytools)
# pacman::p_load( kableExtra, moments, ggpubr, formattable, gridExtra)
# 
# pacman::p_load(glue, corrplot, sessioninfo, readxl, writexl, ggthemes)
# 
# pacman::p_load(patchwork, qqplotr, plotly, lmtest, olsrr, gglm,
#                tidymodels)


# USANDO R BASE ----
## Q1 ----
{
### DADOS 1 ----

dados_brutos1 <- read.csv("dados/caschool.csv")

dplyr::glimpse(dados_brutos1)

dados1 <- dados_brutos1|>
  janitor::clean_names()

### item a ----
dados1[-1]|>
  summary()

hist(dados1$str) #, xlab="Pregnant", ylab="Frequência", main="")
hist(dados1$math_scr)


### item b ----
plot(dados1$math_scr, dados1$str, 
     # pch=23, 
     bty="n", cex=1.3, xlab="", ylab="")
box(bty="l")
title(main = "Diagrama de dispersão entre MATH-SCR e STR", 
      xlab = "MATH-SCR", ylab = "STR", 
      font.main = 3)

# Gráfico utilizado
plot(dados1$str, dados1$math_scr, 
     # pch=23, 
     bty="n", cex=1.3, xlab="", ylab="")
box(bty="l")
title(main = "Diagrama de dispersão entre MATH-SCR e STR", 
      xlab = "STR", ylab = "MATH-SCR", 
      font.main = 3)

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

stats::cor(dados1$math_scr, dados1$str)
}

## Q2 ----
{

### DADOS 2 ----
dados_brutos2 <- read.table('dados/pima.ascii', sep = "", head=T)

dplyr::glimpse(dados_brutos2)

head(dados_brutos2)|>
  kbl(
    caption = "Tabela 1: Apresentação parcial do conjunto de dados.",
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F
  )|>
  kable_material(c("striped", "hover", "condensed"))


### item a.1 ----
summary(dados_brutos2)

dados_brutos2|>
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


### item a.2 ----

#### Histograma ----

# par(mfrow=c(2,4))
# par(mfrow=c(2,4), las=1)
# hist(dados_brutos2$pregnant, xlab="Pregnant", ylab="Frequência", main="")
# hist(dados_brutos2$diastolic, xlab="Diastolic", ylab="Frequência", main="")
# hist(dados_brutos2$triceps, xlab="Triceps", ylab="Frequência", main="")
# hist(dados_brutos2$glucose, xlab="Glucose", ylab="Frequência", main="")
# hist(dados_brutos2$insulin, xlab="Insulin", ylab="Frequência", main="")
# hist(dados_brutos2$bmi, xlab="BMI", ylab="Frequência", main="")
# hist(dados_brutos2$age, xlab="Age", ylab="Frequência", main="")
# hist(dados_brutos2$diabetes, xlab="Diabetes", ylab="Frequência", main="")

par(mfrow = c(2, 4), 
    las = 1, 
    mar = c(4, 4, 3, 1.5), 
    oma = c(1, 1, 3, 1))
{
# hist(dados2$pregnant, xlab="Pregnant", ylab="Frequência", main="")
hist(dados_brutos2$pregnant, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Pregnant", 
      xlab="Pregnant", ylab="Frequência", 
      font.main = 3)
# hist(dados_brutos2$diastolic, xlab="Diastolic", ylab="Frequência", main="")
hist(dados_brutos2$diastolic, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Diastolic", 
      xlab="Diastolic", ylab="Frequência", 
      font.main = 3)
# hist(dados_brutos2$triceps, xlab="Triceps", ylab="", main="")
hist(dados_brutos2$triceps, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Triceps", 
      xlab="Triceps", ylab="Frequência", 
      font.main = 3)
# hist(dados_brutos2$glucose, xlab="Glucose", ylab="Frequência", main="")
hist(dados_brutos2$glucose, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Glucose", 
      xlab="Glucose", ylab="Frequência", 
      font.main = 3)
hist(dados_brutos2$insulin, xlab="Insulin", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável Insulin", 
      xlab="Insulin", ylab="Frequência", 
      font.main = 3)
hist(dados_brutos2$bmi, xlab="BMI", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável BMI", 
      xlab="BMI", ylab="Frequência", 
      font.main = 3)
hist(dados_brutos2$age, xlab="Age", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável Age", 
      xlab="Age", ylab="Frequência", 
      font.main = 3)
hist(dados_brutos2$diabetes, xlab="Diabetes", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável Diabetes", 
      xlab="Diabetes", ylab="Frequência", 
      font.main = 3)

mtext(side = 3, text = "Histograma das Variáveis com dados faltantes representados por zeros", outer = T)
}


### item b.1 ----

# pima$diastolic[pima$diastolic == 0] = NA
# dados_brutos2$pregnant[dados_brutos2$pregnant == 0] = NA
# dados_brutos2$glucose[dados_brutos2$glucose == 0] = NA
# dados_brutos2$diastolic[dados_brutos2$diastolic == 0] = NA
# dados_brutos2$triceps[dados_brutos2$triceps == 0] = NA
# dados_brutos2$insulin[dados_brutos2$insulin == 0] = NA
# dados_brutos2$bmi[dados_brutos2$bmi == 0] = NA
# dados_brutos2$diabetes[dados_brutos2$diabetes == 0] = NA
# dados_brutos2$age[dados_brutos2$age == 0] = NA
# # dados_brutos2$test[dados_brutos2$test == 0] = NA

dados2 <- dados_brutos2 |> 
  dplyr::mutate(
    # pregnant = dplyr::na_if(pregnant, 0),
    glucose = dplyr::na_if(glucose, 0),
    diastolic = dplyr::na_if(diastolic, 0),
    triceps = dplyr::na_if(triceps, 0),
    insulin = dplyr::na_if(insulin, 0),
    bmi = dplyr::na_if(bmi, 0),
    diabetes = dplyr::na_if(diabetes, 0),
    age = dplyr::na_if(age, 0)
    )

# --==--==
quantos.na <-
  plyr::colwise(function(x) sum(is.na(x)))
# --==--==

quantos.na(dados_brutos2)

dados2 |> 
  dplyr::select(glucose, diastolic, triceps, insulin, bmi) |> 
  quantos.na() |>
  kbl(
    caption = "Quantitativo de dados faltante.",
    digits = 2,
    format.args=list(big.mark=".", decimal.mark=","),
    align = "c", 
    row.names = F
  )|>
  kable_material(c("striped", "hover", "condensed"),
    full_width = F
  )|> 
  kable_material()
  

### item b.2 ----

dados2|>
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
  kable_material(c("striped", "hover", "condensed"),
  # kadle_styling(
  #   # dootstrap_options = c("striped", "hover", "condensed", "responsive"),
  #   dootstrap_options = c("striped", "hover"),
    full_width = F
  #   fixed_thead = T # Fixa o cadeçalho ao rolar a tadela.
  )|> 
  footnote(general = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais - EUA") |> 
  kable_material()

par(mfrow=c(2,4), las=1)

#### Histograma ----

par(mfrow = c(2, 4), 
    las = 1, 
    mar = c(4, 4, 3, 1.5), 
    oma = c(1, 1, 3, 1))

{
# hist(dados2$pregnant, xlab="Pregnant", ylab="Frequência", main="")
hist(dados2$pregnant, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Pregnant", 
      xlab="Pregnant", ylab="Frequência", 
      font.main = 3)
# hist(dados2$diastolic, xlab="Diastolic", ylab="Frequência", main="")
hist(dados2$diastolic, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Diastolic", 
      xlab="Diastolic", ylab="Frequência", 
      font.main = 3)
# hist(dados2$triceps, xlab="Triceps", ylab="", main="")
hist(dados2$triceps, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Triceps", 
      xlab="Triceps", ylab="Frequência", 
      font.main = 3)
# hist(dados2$glucose, xlab="Glucose", ylab="Frequência", main="")
hist(dados2$glucose, xlab="", ylab="", main="")
box(bty="l")
title(main = "Variável Glucose", 
      xlab="Glucose", ylab="Frequência", 
      font.main = 3)
hist(dados2$insulin, xlab="Insulin", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável Insulin", 
      xlab="Insulin", ylab="Frequência", 
      font.main = 3)
hist(dados2$bmi, xlab="BMI", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável BMI", 
      xlab="BMI", ylab="Frequência", 
      font.main = 3)
hist(dados2$age, xlab="Age", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável Age", 
      xlab="Age", ylab="Frequência", 
      font.main = 3)
hist(dados2$diabetes, xlab="Diabetes", ylab="Frequência", main="")
box(bty="l")
title(main = "Variável Diabetes", 
      xlab="Diabetes", ylab="Frequência", 
      font.main = 3)

mtext(side = 3, text = "Histograma das Variáveis com remoção dos dados faltantes", outer = T)

}

par(mfrow = c(1, 1), 
    las = 1, 
    mar = c(5.1, 4.1, 4.1, 2.1), 
    oma = c(0, 0, 0, 0))

dados_brutos2$test = factor(dados_brutos2$test)

### item c.1 ----

plot(dados2) # Mostra um painel com graficos de dispersão de todas as variáveis.

### item c.2 ----

par(mfrow = c(2, 4), 
    las = 1, 
    mar = c(4, 4, 3, 1.5), 
    oma = c(1, 1, 3, 1))

plot(x = dados2$pregnant, y=dados2$diabetes, 
     # main = "Diabetes X Pregnant", 
     xlab="Pregnant", ylab="Diabetes")
plot(x = dados2$glucose, y=dados2$diabetes,
     # main = "Diabetes X Glucose", 
     xlab="Glucose", ylab="Diabetes")

plot(x = dados2$diastolic, y=dados2$diabetes,
     # main = "Diabetes X Diastolic", 
     xlab="Diastolic", ylab="Diabetes")

plot(x = dados2$triceps, y=dados2$diabetes,
     xlab="Triceps", ylab="Diabetes")

plot(x = dados2$insulin, y=dados2$diabetes,
     xlab="Insulin", ylab="Diabetes")

plot(x = dados2$bmi, y=dados2$diabetes,
     xlab="BMI", ylab="Diabetes")

plot(x = dados2$age, y=dados2$diabetes,
     xlab="Age", ylab="Diabetes")

mtext(side = 3, text = "Disgramas de dispersão entre a variável Diabetes e as demais variáveis", outer = T)


### item d ----

dados2 |> 
  # dplyr::group_by(test)
  geom_boxplot()

# b1 <- dados2|>
#   ggplot2::ggplot(aes(x = factor(test), y = pregnant))+
#   geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
#   labs(
#     title = "Pregnant",
#     x = "Test",
#     y = "Pregnant"
#   )


b1 <- dados2|>
  ggplot2::ggplot(aes(x = factor(test), y = glucose))+
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Glucose",
    x = "Test",
    y = "Glucose"
  )


b2 <- dados2|>
  ggplot2::ggplot(aes(x = factor(test), y = diastolic))+
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Diastolic",
    x = "Test",
    y = "Diastolic"
  )


b3 <- dados2|>
  ggplot2::ggplot(aes(x = factor(test), y = triceps))+
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Triceps",
    x = "Test",
    y = "Triceps"
  )


b4 <- dados2|>
  ggplot2::ggplot(aes(x = factor(test), y = insulin))+
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Insulin",
    x = "Test",
    y = "Insulin"
  )


b5 <- dados2|>
  ggplot2::ggplot(aes(x = factor(test), y = bmi))+
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "BMI",
    x = "Test",
    y = "BMI"
  )

b6 <- dados2|>
  ggplot2::ggplot(aes(x = factor(test), y = diabetes))+
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Diabetes",
    x = "Test",
    y = "Diabetes"
  )

b7 <- dados2|>
  ggplot2::ggplot(aes(x = factor(test), y = age))+
  geom_boxplot(col="darkblue", fill="skyblue", alpha = 0.5)+
  labs(
    title = "Age",
    x = "Test",
    y = "Age"
  )


b1 + b2 + b3 + b4 +
  plot_layout(ncol = 2) + 
  plot_annotation(
    title = "Figura 6: BoxPlot das variáveis em análise.",
    # caption = "Fonte: StatLib - Carnegie Mellon University",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":") &
  theme_minimal(base_size = 7) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 5, hjust = 0, vjust = -0.4))

b5 + b6 + b7 +
  plot_layout(ncol = 2) + 
  plot_annotation(
    # title = "Figura 7: BoxPlot das variáveis em análise.",
    caption = "Fonte: Instituto Nacional de Diabetes e de Doenças Digestivas e Renais",
    tag_levels = c("A", "1"), tag_prefix = "Sub Fig. ", tag_sep = ".",
    tag_suffix = ":") &
  theme_minimal(base_size = 7) &
  theme(
    plot.tag.position = c(0, 1),
    plot.tag = element_text(size = 5, hjust = 0, vjust = -0.4))

}








# FIM ----


}

var(dados_brutos2$diastolic,
    na.rm=TRUE)

# Tutorial: https://vanderleidebastiani.github.io/tutoriais/Graficos_com_R.html#Pain%C3%A9is_gr%C3%A1ficos


# las: Altera a orientação dos números dos eixos.


plot(diabetes ~ test, data=dados_brutos2, xlab="Test", ylab="Diabetes")
box(bty="l")

# plot(age ~ test, data=dados_brutos2, xlab="Idade", ylab="Diabetes")
# 
# par(mfrow=c(1,1))









# K*\L\c5zmfb



# USANDO PKT ----

## Questão 1 ----
### Item a ----

dados1|>
  dplyr::select(-observation_number) |> 
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
    caption = "Tabela 1: Medidas tendência central e dispersão.",
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
  footnote(general = "Fonte: California Standardized Testing and Reporting (STAR)") |>
  kable_material()
# add_header_adove(c("Características", "Medidas de Tendência Central e Variadilidade" = 8))

