## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ----instalando_paquetes------------------------------------------------------------------------------------------------------------------------------
## Install packages:
# Usa este código para instalar los paquetes que te falten 
# install.packages("tidyverse")
# install.packages("cli")
# install.packages("haven")
# install.packages("stargazer")
# install.packages("fixest")

# Para esta clase
# install.packages("modelsummary")
# install.packages("estimatr")
# install.packages("rdd")
# install.packages("rdrobust")
# install.packages("rddensity")


## ----abriendo_liberias--------------------------------------------------------------------------------------------------------------------------------

library(haven)
library(tidyverse)
library(estimatr)
library(fixest)

# This chapter only
library(modelsummary)
library(rdd)
library(rdrobust)
library(rddensity)

# read_data function  (la usamos para descargar datos del repositorio github de Scott Cuningham)
read_data <- function(df) {
  full_path <- paste0("https://raw.github.com/scunning1975/mixtape/master/", df)
  return(haven::read_dta(full_path))
}




## ----leyendo datos------------------------------------------------------------------------------------------------------------------------------------
### Leamos la data y guardémosla en un objeto de R
#### Fijate que la estás leyendo directamente desde un repositorio (ver cómo creamos la función read_data()), asi que necesitas conexión a intenet para leerla

lmb_data <- read_data("lmb-data.dta") 



## ----vistazo_1----------------------------------------------------------------------------------------------------------------------------------------
## Démosle un vistazo rápido a la data
### Tiene un montón de variables, pero no se alarmen: Usarémos unas pocas
head(lmb_data %>% 
          select(id, n,name,year,score,
                 state, district, incmbncy,
                 democrat,
                 demvote, repvote,
                 demvoteshare,lagdemvoteshare,
                 democrat,lagdemocrat))



## ----vistazo_2----------------------------------------------------------------------------------------------------------------------------------------
## Veamos las estadísticas descriptivas de estas variables. No red flags
summary(lmb_data %>% 
          select(id, year,state, district, incmbncy,
                 demvote, repvote,
                 score,demvoteshare,lagdemvoteshare,
                 democrat,lagdemocrat))


## ----resultados_originales----------------------------------------------------------------------------------------------------------------------------
## Fíjense que estamos usando un nuevo comando llamado lm_roboust
## Este comando sirve para calcular errores estándar robustos o para agrupar (cluster) errores estándar. No se preocupen por el motivo detrás de esto por el momento.

lmb_subset <- lmb_data %>% 
  filter(lagdemvoteshare>.48 & lagdemvoteshare<.52) 

lm_1 <- feols(score ~ lagdemocrat, data = lmb_subset)
lm_2 <- feols(score ~ democrat, data = lmb_subset)
lm_3 <- feols(democrat ~ lagdemocrat, data = lmb_subset)

#"Original results based on ADA Scores -- Close Elections Sample"

msummary(list("ADA Score t+1"=lm_1,
              "ADA Score t"=lm_2,
              "Democrat t+1"=lm_3), 
         vcov = 'robust',
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_rename = c('lagdemocrat' = 'Demo', 'democrat' = 'Demo'),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"))



## ----resultados_all_data_simple-----------------------------------------------------------------------------------------------------------------------
#using all data (note data used is lmb_data, not lmb_subset)
lm_1 <- feols(score ~ lagdemocrat, data = lmb_data)
lm_2 <- feols(score ~ democrat, data = lmb_data)
lm_3 <- feols(democrat ~ lagdemocrat, data = lmb_data)

# "Results based on ADA Scores -- Full Sample"
msummary(list("ADA Score t+1"=lm_1,
              "ADA Score t"=lm_2,
              "Democrat t+1"=lm_3), 
         vcov = 'robust',
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_rename = c('lagdemocrat' = 'Demo', 'democrat' = 'Demo'),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"))



## ----resultados_all_data_centered---------------------------------------------------------------------------------------------------------------------
## Actualicemos la variable data con una nueva columna que sea el porcentaje que obtuvieron los demócratas centrado en cero
lmb_data<-lmb_data %>%
  mutate(demvoteshare_c=demvoteshare-0.5)

### Pregunta de R: Fijate que actualizamos lmb_data. ¿Crees que lmb_subset se actualizó también? ¿Si o no?


lm_1 <- feols(score ~ lagdemocrat + demvoteshare_c, data = lmb_data, clusters = id)
lm_2 <- feols(score ~ democrat + demvoteshare_c, data = lmb_data, clusters = id)
lm_3 <- feols(democrat ~ lagdemocrat + demvoteshare_c, data = lmb_data, clusters = id)

#"Results based on ADA Scores -- Full Sample-- adding centered democrats' vote share"
msummary(list("ADA Score t+1"=lm_1,
              "ADA Score t"=lm_2,
              "Democrat t+1"=lm_3), 
         vcov = 'robust',
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_rename = c('lagdemocrat' = 'Demo', 'democrat' = 'Demo'),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"))



## ----resultados_all_data_interactuado-----------------------------------------------------------------------------------------------------------------
lm_1 <- feols(score ~ lagdemocrat*demvoteshare_c, 
                  data = lmb_data)
lm_2 <- feols(score ~ democrat*demvoteshare_c, 
                  data = lmb_data)
lm_3 <- feols(democrat ~ lagdemocrat*demvoteshare_c, 
                  data = lmb_data)

# "Results based on ADA Scores -- Full Sample with linear interactions"
msummary(list("ADA Score t+1"=lm_1,
              "ADA Score t"=lm_2,
              "Democrat t+1"=lm_3), 
         vcov = 'robust',
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_rename = c('lagdemocrat' = 'Demo', 'democrat' = 'Demo',
                         'demvoteshare_c'='Share_c',
                         'democrat:demvoteshare_c'='Demo X Share_c',
                         'lagdemocrat:demvoteshare_c'='Demo X Share_c'),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"))



## ----resultados_all_data_polinomial-------------------------------------------------------------------------------------------------------------------

lmb_data <- lmb_data %>% 
  mutate(demvoteshare_sq = demvoteshare_c^2)

lm_1 <- feols(score ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_sq, 
                  data = lmb_data)
lm_2 <- feols(score ~ democrat*demvoteshare_c + democrat*demvoteshare_sq, 
                  data = lmb_data)
lm_3 <- feols(democrat ~ lagdemocrat*demvoteshare_c + lagdemocrat*demvoteshare_sq, 
                  data = lmb_data)


# "Results based on ADA Scores -- Full Sample with linear and quadratic interactions"
msummary(list("ADA Score t+1"=lm_1,
              "ADA Score t"=lm_2,
              "Democrat t+1"=lm_3), 
         vcov = 'robust',
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_rename = c('lagdemocrat' = 'Demo', 'democrat' = 'Demo',
                         'demvoteshare_c'='Share_c',
                         'democrat:demvoteshare_c'='Demo X Share_c',
                         'lagdemocrat:demvoteshare_c'='Demo X Share_c',
                         'democrat:demvoteshare_sq'='Demo X Share_c^2',
                         'lagdemocrat:demvoteshare_sq'='Demo X Share_c^2'),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"))



## ----graficas, fig.width=12,fig.height=10-------------------------------------------------------------------------------------------------------------
## Para visualizar la discontinuidad del ADA score calculemos su promedio en cada punto porcentual del running variable. Es decir, elegimos 100 ventanas  de longitud 0.01 cada una. Estos datos serviran para la capa de esitmados discretos.

agg_lmb_data<-lmb_data %>% 
  mutate(categorias=cut(lagdemvoteshare, 100)) %>% 
  group_by(categorias) %>%
  summarise(score=mean(score),
            pctblack=mean(pctblack, na.rm = T),
            medianincome=mean(medianincome, na.rm = T),
            votingpop=mean(votingpop, na.rm = T),
            lagdemvoteshare=min(lagdemvoteshare)) %>% 
  ungroup() 

## Acá les dejo un código para hacerlo con los comandos nativos de R.
# demmeans <- split(lmb_data$score, cut(lmb_data$lagdemvoteshare, 100)) %>% 
#   lapply(mean) %>% 
#   unlist()
# 
# agg_lmb_data <- data.frame(score = demmeans, lagdemvoteshare = seq(0.01,1, by = 0.01))

#plotting
lmb_data <- lmb_data %>% 
  mutate(gg_group = case_when(lagdemvoteshare > 0.5 ~ 1, TRUE ~ 0))

## Lineal
p0<-ggplot(lmb_data , aes(lagdemvoteshare, score)) +
  # la funcion geom point añade la capa de estimados locales, 
  # fijate que cada capa puede usar un data.fram diferente, siempre y cuando tengan las columnas que se necesitan
  # en el gráfico (con igual nombre y formato)
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  geom_point(aes(x = lagdemvoteshare, y = score),fun = mean, colour = "red", size = 2, 
               data=agg_lmb_data%>% 
             filter(lagdemvoteshare>.49 & lagdemvoteshare<.51))+
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5) +
  labs(subtitle = "Comparando el promedio de ambos lados del corte",
       x = "Porcentaje de votos demócratas, t-1",
       y= "Ada Score")

## Lineal
p1<-ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  # stat_smooth permite añadir un ajuste lineal a los datos. El argumento method the sirve para especificar el método
  # Fijate que usamos el argumento group. Esto nor sirva para especificar los grupos de datos que cada capa debe tratar por separado. A efectos de la regresion, esto es lo que define la interaccion
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)+
  labs(subtitle = "Especificación lineal",
       x = "Porcentaje de votos demócratas, t-1",
       y= "Ada Score")

## Polinomio grado 2  
p2<-ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "lm", 
              formula = y ~ x + I(x^2)) +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)+
  labs(subtitle = "Especificación polínómica",
       x = "Porcentaje de votos demócratas, t-1",
       y= "Ada Score")

# Método densidad local "LOESS"
p3<-ggplot(lmb_data, aes(lagdemvoteshare, score)) +
  geom_point(aes(x = lagdemvoteshare, y = score), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, score, group = gg_group), method = "loess") +
  xlim(0,1) + ylim(0,100) +
  geom_vline(xintercept = 0.5)+
  labs(subtitle = "Usando Loess smoothing",
       x = "Porcentaje de votos demócratas, t-1",
       y= "Ada Score")

library(patchwork)
(p0+p1)/(p2+p3)

## Desafío para la casa. Hagan lo mismo para ver el impacto sobre las variables democrats (porcentaje de democratas electos en el periodo t)



## ----graficas_covariates, fig.width=12,fig.height=6---------------------------------------------------------------------------------------------------
cov_1<-ggplot(lmb_data, aes(lagdemvoteshare, log(medianincome))) +
  geom_point(aes(x = lagdemvoteshare, y = log(medianincome)), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, y=log(medianincome), group = gg_group), method = "loess") +
  xlim(0,1) +
  geom_vline(xintercept = 0.5)+
  labs(subtitle = "Diferencias en ingreso usando Loess smoothing",
       x = "Porcentaje de votos demócratas, t-1",
       y= "Ingresio mediano (in logs)")
cov_2<-ggplot(lmb_data, aes(lagdemvoteshare, votingpop)) +
  geom_point(aes(x = lagdemvoteshare, y = votingpop), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, y=votingpop, group = gg_group), method = "loess") +
  xlim(0,1) +
  geom_vline(xintercept = 0.5)+
  labs(subtitle = "Diferencias en población usando Loess smoothing",
       x = "Porcentaje de votos demócratas, t-1",
       y= "Población en edad electoral")

cov_3<-ggplot(lmb_data, aes(lagdemvoteshare, pctblack)) +
  geom_point(aes(x = lagdemvoteshare, y = pctblack), data = agg_lmb_data) +
  stat_smooth(aes(lagdemvoteshare, y=pctblack, group = gg_group), method = "loess") +
  xlim(0,1) +
  geom_vline(xintercept = 0.5)+
  labs(subtitle = "Diferencias raciales usando Loess smoothing",
       x = "Porcentaje de votos demócratas, t-1",
       y= "Porcentaje de afroamericano")

cov_1+cov_2/ cov_3

