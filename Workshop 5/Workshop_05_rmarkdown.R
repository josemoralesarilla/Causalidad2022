## ----setup, include=FALSE----------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, message = FALSE, warning = FALSE)


## ---- message=FALSE, warning=FALSE-------------------------------------------------------------------
# *Agradecimiento: Este tutorial se basa ampliamente en materiales preparados por Wendy Brau y Gastón Garcia, asistentes del Departamento de Economía de la Universidad de San Andrés (UDESA), cátedra de Econometría Avanzada.*

# install.packages('AER')
# install.packages('tidyverse')

library(haven) # para abrir bases de datos en formato .dta
library(stargazer) # para tablas 

library(tidyverse) # librer?a muy usada para manipular los datos y hacer gr?ficos
library(AER) # para usar el comando de variables instrumentales




## ----------------------------------------------------------------------------------------------------
# Limpio: (borro los elementos del environment)
rm(list=ls())

# Abro la base de datos:
qob_data <- read_dta("data/QOB.dta")

# Que variables tenemos
names(qob_data)

# Estadisticas descriptivas generales
summary(qob_data)


## ----------------------------------------------------------------------------------------------------
# A.1 Cuadro con estad\'isticas descriptivas 
stats<-qob_data %>% 
  filter(yob >=30 & yob <=49) %>%
  mutate(decade=case_when(yob>=30 & yob<=39~"30's",
                          TRUE ~ "40's"),
         wage=exp(lwage)) %>%
  group_by(decade) %>% 
  mutate(normalized_educ=(educ-mean(educ))/sd(educ),
         normalized_wage=(wage-mean(wage))/sd(wage)) %>% 
  group_by(decade,qob) %>% 
  summarise(mean_educ=mean(educ),
            educ_deviations_from_mean=mean(normalized_educ),
            mean_wage=mean(wage),
            wage_deviations_from_mean=mean(normalized_wage))

library(gt)
gt(stats) %>% 
  tab_header(
    title = md("Anos de educacion promedio por trimestre de nacimiento")
  ) %>%
  gt::cols_label(
    decade="Decada",
    qob="Trimestre de nacimiento",
    mean_educ="Anos de educacion promedio",
    educ_deviations_from_mean = "Anos de educacion (desvios estandar de la media)",
    mean_wage="Salario promedio",
    wage_deviations_from_mean = "Salario (desvios estandar de la media)"
  )



## ----------------------------------------------------------------------------------------------------
# Summarize the average wages by age:
data_age <- qob_data %>%
  group_by(qob, yob) %>%
  summarise(lnw = mean(lwage), m_educ = mean(educ)) %>%
  mutate(q4 = (qob == 4)) %>% 
  arrange( yob, qob)

p1<-ggplot(data_age, aes(x = yob + (qob - 1) / 4, y = m_educ)) +
      geom_line() +
      geom_label(mapping = aes(label = qob, color = q4)) +
      scale_x_continuous("Year of birth") +
      scale_y_continuous("Log weekly wages") +
      theme(legend.position = "none")
    
p2<-ggplot(data_age, aes(x = yob + (qob - 1) / 4, y = lnw)) +
      geom_line() +
      geom_label(mapping = aes(label = qob, color = q4)) +
      scale_x_continuous("Year of birth" ) +
      scale_y_continuous("Log weekly wages") +
      theme(legend.position = "none") 
library(patchwork) #sirve para pegar graficas de lado a lado. 
p1+p2


## ----ols_and_filter, echo=TRUE-----------------------------------------------------------------------
qob_data_30<-qob_data %>%
  filter(yob >=30 & yob <=39)

ols<-lm(lwage~educ + ageq+ageqsq+race+married+smsa, data=qob_data_30) 

stargazer(ols, type="text",
          column.labels = c("OLS"),
          dep.var.labels=c("Log wage")
)




## ---- create_instruments-----------------------------------------------------------------------------
qob_data_30<-qob_data_30 %>% 
  mutate(z1=ifelse(qob==1,1,0),
         z2=ifelse(qob==2,1,0),
         z3=ifelse(qob==3,1,0))



## ----ftest-------------------------------------------------------------------------------------------
### F-test output: Recuerden que lo vimos en la primera clase
f_test<-lm(educ~z1+z2+z3, data = qob_data_30) 

f_test%>% 
  summary()


## ----fs_school---------------------------------------------------------------------------------------
### Output first stage
first_stage<-lm(educ~z1+z2+z3+ageq+ageqsq+race+married+smsa, data = qob_data_30)
summary(first_stage)


## ----ss_fs_school------------------------------------------------------------------------------------
### Output second stage

qob_data_30$educ_hat_over<-first_stage$fitted.values

second_stage_over<-lm(lwage~educ_hat_over+ ageq + ageqsq + race + married + smsa, data = qob_data_30)



## ----ivreg_school------------------------------------------------------------------------------------
### Output IVreg

iv_fit <- ivreg(lwage ~ educ+ageq + ageqsq + race + married + smsa | 
                  z1 + z2 + z3 + ageq + ageqsq + race + married + smsa ,
                data = qob_data_30) 




## ----resutls_school,  out.width=20-------------------------------------------------------------------
stargazer(first_stage,second_stage_over,iv_fit, type="text",
          column.labels = c("First stage", "Second stage", "Estimador IV"),
          dep.var.labels=c("Log wage")
          # ,
          # out="models_2sls.txt")
)




## ----------------------------------------------------------------------------------------------------
# Limpio: (borro los elementos del environment)
rm(list=ls())

# Abro la base de datos:
data <- read_dta("data/pesoalnacer.dta")

# Seteo 2 decimales y sin notaci?n cient?fica
options(scipen=100)
options(digits=2)

# Que variables tenemos
names(data)



## ----summary_rbase-----------------------------------------------------------------------------------
# Lo mas basico
summary(data)

# Agrupando por una variable categ?rica

# R base
aggregate(data, by=list(data$male), mean, na.rm=T)
aggregate(data[c('lbwght','motheduc')], by=list(data$male), mean, na.rm=T)

### Par?ntesis:
### na.rm es un argumento para no considerar los NA al hacer el c?lculo
mean(c(1,2,NA,3))
mean(c(1,2,NA,3), na.rm=T)




## ----summary_tidyverse-------------------------------------------------------------------------------
# tidyverse 
# Media de todas las variables por grupo:
data %>%
  group_by(male) %>%
  summarise_all(mean, na.rm=T)

# Mas especifico:
data %>%
  group_by(male) %>%
  summarise(peso = mean(lbwght, na.rm=T),
            peso_sd = var(lbwght, na.rm=T)  ) # podemos definir distintas funciones a aplicar a cada grupo

# Agrupo por precio de cigarrillo
data$caros <-  ifelse(data$cigprice>mean(data$cigprice), 'caros', 'baratos') # creo categorica a partir de numerica

data %>%
  group_by(male, caros) %>% # y agrupo por dos variables
  summarise(peso = mean(lbwght, na.rm=T),
            motheduc = mean(motheduc, na.rm=T))



## ----------------------------------------------------------------------------------------------------
### Podemos guardarlo en un objeto y usar stargazer con summary=FALSE para guardar la tabla
# Creemos tablas usando stargazer. Este es un paquete muy flexible para crear tabla, sobretodo para publicaciones impresas, como sus potenciales trabajos finales y/o tesis de grado.
tabla <-  data %>%
  group_by(male, caros) %>%
  summarise(peso = mean(lbwght, na.rm=T),
            educmadre = mean(motheduc, na.rm=T))
tabla
stargazer(tabla, type='text', summary = FALSE, digits = 2) 



## ----filter------------------------------------------------------------------------------------------
# Si queremos quedarnos con un subconjunto de los datos ----
data <- subset(data, lfaminc >= 0)

# Otra forma, con tidyverse
data <-  data %>%
  filter(lfaminc >= 0)


## ----mco---------------------------------------------------------------------------------------------
# MCO 
lm.fit <- lm(lbwght ~ packs + male + parity + lfaminc, data=data) 
summary(lm.fit) # logaritmo: 1 paquete afecta en x% al peso al nacer
stargazer(lm.fit, type="text")



## ----------------------------------------------------------------------------------------------------
# IV 
iv.fit <- ivreg(lbwght ~ packs + male + parity + lfaminc | 
                  male + parity + lfaminc + cigprice, # instrumentos (cada variable ex?gena es su propio instrumento)
                data = data) 
summary(iv.fit) 



## ----iv----------------------------------------------------------------------------------------------
#MC2E "a mano"
#First stage:
# Nota que lo se usa tanto el instrumento principal como todos los demas regresores.
first.stage <- lm(packs ~ cigprice + male + parity + lfaminc, data=data)

summary(first.stage)

data$packs_hat <- first.stage$fitted.values

## Instrumentos dbiles Regla del pulgar
## Stock, J.H. & Watson M. (2011). Introduction to econometrics. 3rd Ed., Pearson Education.
## Stock J & Yogo M. (2005). Testing for Weak Instruments in Linear IV Regression. In:
##     Identification and Inference for Econometric Models. New York: Cambridge University Press. pp. 80-108.

#Segunda etapa:
second_stage <- lm(lbwght ~ packs_hat + male + parity + lfaminc, data = data)

#Juntemos todo:
stargazer(first.stage, second_stage, iv.fit, type="text",
          column.labels = c("First Stage", "Second Stage", "Estimador IV"),
          dep.var.labels=c("lbwght"), out="models_2sls.txt")


