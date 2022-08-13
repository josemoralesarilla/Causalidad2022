## ----setup, include=FALSE-------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, error = FALSE, message=FALSE)


## ----librerias------------------------------------------------------------------------------------
library(tidyverse) # Hablamos de esto en clase 1. Ya saben para que es. 
library(haven)     # Para leer archivos en formato dta. (el formato preferido de Stata)
library(huxtable)  # Para tablas de regresion en HTML (ver otras opciones para distintos formatos i.e. stargazzer para LaTeX
library(patchwork) # juntar graficos lado a lado
library(wooldridge) # datos para estudios
library(estimatr)


## -------------------------------------------------------------------------------------------------
#EXPERIMENT (experimental)
data(jtrain2)
jtrain2 %>% group_by(train) %>% summarize(wage = mean(re78))


## -------------------------------------------------------------------------------------------------
#BY CHOICE (non experimental/observacional)
data(jtrain3)
jtrain3 %>% group_by(train) %>% summarize(wage = mean(re78))



## ---- manzanas_peras_1----------------------------------------------------------------------------
select(jtrain2,re75,re78) %>%  summary()


## ---- manzanas_peras_2----------------------------------------------------------------------------
select(jtrain3,re75,re78) %>%  summary()


## ---- control_manual_1----------------------------------------------------------------------------
jtrain2 %>% 
  filter(re75<2) %>%
  group_by(train) %>% 
  summarize(wage = mean(re78))



## ---- control_manual_2----------------------------------------------------------------------------

jtrain3 %>% 
  filter(re75<2.) %>% 
  group_by(train) %>% 
  summarize(wage = mean(re78))



## ----regresion_1----------------------------------------------------------------------------------
# Asi creo las regresiones, el comando lm() no pertenece a ningun paquete, sino que esta en R base
experiment_lm<-lm(re78~ train,, data=jtrain2)
no_control_lm<-lm(re78~ train, data = jtrain3) 
wage_control_lm<-lm(re78~ train+re75, data = jtrain3) 

huxreg( experiment_lm,no_control_lm,wage_control_lm,
       statistics = c("N. obs." = "nobs", 
      "R squared" = "r.squared", "F statistic" = "statistic",
      "P value" = "p.value")) 



## ---- regresion_ejemplo_1-------------------------------------------------------------------------
lm(re78~ train, data=jtrain2)


## ---- regresion_ejemplo_2-------------------------------------------------------------------------
revisa_lm<-lm(re78~ train, data=jtrain2)

# revisa_lm es una clase particular de objeto llamada lm (Linear model)
revisa_lm %>% class()

# Si imprimes los contenidos del objeto salen a relucir varias piezas
revisa_lm %>% names()


## ---- examina_regresion---------------------------------------------------------------------------
revisa_lm


## ----presenta_regresion---------------------------------------------------------------------------
huxreg(revisa_lm)


## ----tidy_regresion-------------------------------------------------------------------------------
# instala el paquete estimatr
estimatr::tidy(revisa_lm)


## ---- pred1, eval = F-----------------------------------------------------------------------------
## revisa_lm$fitted.values






## ----mixtape_1------------------------------------------------------------------------------------

# Creamos una funcion que descarga nuestros datos desde un repositorio la interenet 
read_data <- function(df) {
  full_path <- paste("https://raw.github.com/scunning1975/mixtape/master/", 
                     df, sep = "")
  
  # full_path contiene la URL donde esta guardada la data
  # el usuario solo debe especificar el nombre de los datos. Si en el repositorio no existen
  # datos con ese nombre, arrojara un error
  
  df <- read_dta(full_path)
  return(df)
}

nsw_dw <- read_data("nsw_mixtape.dta")

# Calculo la media de los tratados
mean1 <- nsw_dw %>% 
  filter(treat == 1) %>% 
  pull(re78) %>% 
  mean()

## Creo columna cuyo unico valor es la media de los tratados
nsw_dw$y1 <- mean1 # con dplry pudieramos hacer algo asi: a nsw_dw %>% mutate(y1=mean1)

# Calculo la media de los no tratados
mean0 <- nsw_dw %>% 
  filter(treat == 0) %>% 
  pull(re78) %>% 
  mean()

## Creo columna cuyo unico valor es la media de los no tratados
nsw_dw$y0 <- mean0

ate <- unique(nsw_dw$y1 - nsw_dw$y0)
ate

## remuevo columnas con los promedios
nsw_dw <- nsw_dw %>% 
  filter(treat == 1) %>% 
  select(-y1, -y0)



## ----mixtape_2_evaluate---------------------------------------------------------------------------
# Descargamos unos datos nuevos del repositorio
# Estos son del Current Population Survey, una encuesta de hogares MENSUAL hecha en EEUU.
# la gente del CPS son iguales a los de jtrain3 (No experimentales)
nsw_dw_cpscontrol <- read_data("cps_mixtape.dta") %>% 
  # usamos bind_rows para juntar los registros experimentales y no experimentales
  bind_rows(nsw_dw) %>% 
  # Creamos variables al cuadrado, interacciones, variables binarias, y mas
  # Sugerencia: Repasen como se interpretan los coeficientes de estas variables en un MCO  
  mutate(agesq = age^2,
         agecube = age^3,
         educsq = educ*educ,
         u74 = case_when(re74 == 0 ~ 1, TRUE ~ 0),
         u75 = case_when(re75 == 0 ~ 1, TRUE ~ 0),
         interaction1 = educ*re74,
         re74sq = re74^2,
         re75sq = re75^2,
         interaction2 = u74*hisp)

# estimating using a LOGIT link function
logit_nsw <- glm(treat ~ age + agesq + agecube + educ + educsq + 
                   marr + nodegree + black + hisp + re74 + re75 + u74 +
                   u75 + interaction1, family = binomial(link = "logit"), 
                 data = nsw_dw_cpscontrol)

## Extraigo las predicciones de probabilidad para cada observacion, que estan guardadas en logit_nsw$fitted.values 
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(pscore = logit_nsw$fitted.values)

## Calculo el promedio del pscore para tratados y no los no tratados  
nsw_dw_cpscontrol %>% 
  group_by(treat) %>% 
  summarise(pscore=mean(pscore))

# Hagamos unos graficos de distribucion tipo histograma
# Primera vez que hacemos uno. 
p1<-nsw_dw_cpscontrol %>% 
  filter(treat == 0) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))+
  labs(subtitle="P score de quienes no asistieron al programa")

p2<-nsw_dw_cpscontrol %>% 
  filter(treat == 1) %>% 
  ggplot() +
  geom_histogram(aes(x = pscore))+
  labs(subtitle="P score de quienes asistieron al programa")

# juntemos ambos graficos lado a lado
library(patchwork)
p1+p2



## ----mixtape_3_weigths----------------------------------------------------------------------------
#- Manual with non-normalized weights using all data

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)

nsw_dw_cpscontrol %>% 
  pull(ht) %>% 
  mean()

#- Manual with normalized weights
# continuation. Store the number of observations
N <- nrow(nsw_dw_cpscontrol)

# Store the sum of the adjusted treatment 
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)

nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/N),
         norm = y1 - y0)


nsw_dw_cpscontrol %>% 
  pull(norm) %>% 
  mean()



## ----mixtape_3_weigths_trim-----------------------------------------------------------------------
#-- trimming propensity score
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  select(-d1, -d0, -y1, -y0, -ht, -norm) %>% 
  filter(!(pscore >= 0.9)) %>% 
  filter(!(pscore <= 0.1))

# Storing number of observations in the data. Now they are fewer
N <- nrow(nsw_dw_cpscontrol)

#- Manual with non-normalized weights using trimmed data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = treat * re78/pscore,
         y0 = (1-treat) * re78/(1-pscore),
         ht = y1 - y0)
nsw_dw_cpscontrol %>% 
  pull(ht) %>% 
  mean()

#- Manual with normalized weights with trimmed data
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(d1 = treat/pscore,
         d0 = (1-treat)/(1-pscore))

s1 <- sum(nsw_dw_cpscontrol$d1)
s0 <- sum(nsw_dw_cpscontrol$d0)
nsw_dw_cpscontrol <- nsw_dw_cpscontrol %>% 
  mutate(y1 = (treat*re78/pscore)/(s1/N),
         y0 = ((1-treat)*re78/(1-pscore))/(s0/N),
         norm = y1 - y0)



nsw_dw_cpscontrol %>% 
  pull(norm) %>% 
  mean()


