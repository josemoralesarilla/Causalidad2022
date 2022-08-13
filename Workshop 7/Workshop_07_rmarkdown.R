## ----setup, include=FALSE----------------------------------------------------------------------------------------------
# Nota de los autores:
## Este documento contiene todo el analisis econometrico necesario para responder las consignas.

knitr::opts_chunk$set(echo = TRUE, 
                      warning = FALSE,
                      message = FALSE)

# 0.0 install required packages -------------------------------------------
# install packages not installed

list.of.packages <- c("readr", "dplyr", "tidyr")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# 0.1 Load packages -------------------------------------------------------
library(readr)
library(ggplot2)
library(patchwork)
library('plm')
library(modelsummary)
library(dplyr)
library(tidyr)

# read dataset:

cornwell_data <- read_csv("data/cornwell.csv")





## ----summary_data------------------------------------------------------------------------------------------------------
## Veamos un pequeno resumen de la data (fijate que tiene mucho mas que 10 columnas, aca te muestro un resumen)
## Red flag: Hay unos valores de probabilidad de prision que estan por encima de uno, mas adelante los removeremos.
skimr::skim(select(cornwell_data,1:10))

## Cercioremonos de que el panel esta completo (rectangularizado)
## Sabemos que hay 7 periodos en la data. Revisaremos si encontramos algun condado que no aparezca 7 veces
cornwell_data %>% 
  group_by(county) %>%
  # la funcion count crea una columna llmada n que muestra la cantidad de filas en la data (o en el grupo)
  count() %>% 
  # nos quedamos con grupos con menos de 7 obs. Bingo! todos tienen 7 obs.
  filter(n<7)


## ----transform_data----------------------------------------------------------------------------------------------------
## Cómo hacer nuestra propia transformación between con dplry?

cornwell_between<-cornwell_data %>% 
  group_by(county) %>% 
  # Usamos una variante de la función summarise. Se acuerdan para que era la función? Nos sirve para collapsar o agregar la data en totales, promedios, o en cualquier función que reciba N valores y devuelva 1 valor (en este caso, 1 valor por grupo).
  # summarise_at es útil cuando queremos usar la misma función sobre un grupo amplio de variables. En este caso lo hacemos para todas exeptuando "year"
  summarise_at(vars(-year), mean,na.rm=T)  



## ----trasnform_data_1--------------------------------------------------------------------------------------------------
## Cómo expresar la data como desviaciones de la media usando dplry?

## Creu una funcion pequena para restarle su propia media a cada variable
recenter <- function(x, na.rm = FALSE) (x - mean(x, na.rm = na.rm)) 

cornwell_within<-cornwell_data %>% 
  group_by(county) %>% 
  # Ahora usaremos mutate para cambiar las variables. Se entiende la diferencia entre summarise y mutate? el primero modifica la cantidad de filas, mientras que el otro modifica la base de datos a nivel de columnas.
  # Podemos agregar nuestra propia función de transformación (la de arriba)
  # (es mas, pudieras escribirla ahi mismo, pero asi me parece que es mas legible)
  mutate_at(vars(-year,-county), recenter, na.rm = TRUE )  
  


## ----estimate_between--------------------------------------------------------------------------------------------------
#-------------------------------------------#
# Between Model (Table 3 Cornwell) ---------#
#-------------------------------------------#

## Primero a mano
BE_model_amano <- lm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell_between)

## Despues con la funcion plm del paquete 'plm' (https://cran.r-project.org/web/packages/plm/plm.pdf)
BE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell_data, index=c("county", "year"), model="between")

## El punto de mostrar dos maneras es que notes que son equivalentes.
msummary(list("Crime rate (metodo manual)"=BE_model_amano,
              "Crime rate (plm package)"=BE_model), 
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_map = c('lprbarr' = 'Log prob. arr.',
                         'lprbconv' = 'Log prob. conv.',
                         'lprbpris'=" Log prob. pris.",
                         'lavgsen'='Log avg. sen.',
                      'urban'="Urban",
                      'west'="West",
                      'central'="Central"),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"))  



## ----chart_betweem_data, fig.width=12,fig.height=10--------------------------------------------------------------------
## Veamos unos graficos simples de la data antes de seguir.

### Primero creemos un data.frame con todos los coeficietnes del modelo. Esto podremos pegarlo despues en la parte del grafico que prefiramos.
coeficientes<-BE_model_amano %>% 
  modelsummary::tidy() %>%
  select(term,estimate)

### Ahora veamos la distribucion conjunta de cada variable y la tasa de criminalidad.
p1<-cornwell_between %>% 
  ggplot(aes(y=lcrmrte,x=lprbarr))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Probability of arrest and crime rate",
       subtitle = paste0("La elasticidad es de: ", round(filter(coeficientes,term=="lprbarr")$estimate,3)," tras aplicar controles"))

p2<-cornwell_between %>% 
  ggplot(aes(y=lcrmrte,x=lprbconv))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Probability of conviction and crime rate",
       subtitle = paste0("La elasticidad es de: ", round(filter(coeficientes,term=="lprbconv")$estimate,3)," tras aplicar controles"))

p3<-cornwell_between %>% 
  ggplot(aes(y=lcrmrte,x=lprbpris))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Probability of imprisionment and crime rate",
       subtitle = paste0("La elasticidad es de: ", round(filter(coeficientes,term=="lprbpris")$estimate,3)," tras aplicar controles"))

p4<-cornwell_between %>% 
  ggplot(aes(y=lcrmrte,x=lavgsen))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(title="Average sentence and crime rate",
       subtitle = paste0("La elasticidad es de: ", round(filter(coeficientes,term=="lavgsen")$estimate,3)," tras aplicar controles"))


(p1+p2)/(p3+p4)+plot_annotation(caption = "Nota: Las elasticidades reportadas en la tabla y en los subtítulos no coinciden con las pendientes de estas rectas ¿Por qué?")





## ----within_models,include=TRUE----------------------------------------------------------------------------------------
#-------------------------------------------#
# Within Model (Table 3 Cornwell) 
#-------------------------------------------#

# Aca usamos los datos within que creeamos arriba. Usamos la funcion de regresion lineal mas sencilla que hay
FE_model_a_mano <- lm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell_within)

# Aca usamos la data original, pero añadimos una variable binaria por cada condado insertando la variable (county) en la formula en formato factor (es muy importante que no esté en formato numerico ¿Por que?) 
FE_model_a_mano_ii <- lm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin+
                    factor(county),
                data = cornwell_within)

# Acá usamos la función plm del paquete plm, especificandole que queremos el estimador within
FE_model <- plm(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin,
                data = cornwell_data, index=c("county", "year"), model="within")
library(fixest)
FE_model_2 <- feols(lcrmrte ~ lprbarr+lprbconv+lprbpris+lavgsen+lpolpc+ldensity+lpctymle+lwcon+lwtuc+lwtrd+
                  lwfir+lwser+lwmfg+lwfed+lwsta+lwloc+west+central+urban+lpctmin | county,
                data = cornwell_data)
# También valdría la pena replicarlo con el paquete fixest

## El punto de mostrar tres maneras es que notes que son equivalentes.
## De las tres, quizás la mas transparente es la segunda, pero es la mas ineficiente.
## Se recomiendo usar bien sea la tercera o la cuarta, sobretodo por la flexibilidad que tendran para ajustar sus errores estandar de alguna manera.

msummary(list("Crime rate (metodo manual)"=FE_model_a_mano,
              "Crime rate (metodo manual 2)"=FE_model_a_mano_ii,
              "Crime rate (plm package)"=FE_model,
              "Crime rate (feols function)"=FE_model_2), 
         stars = c('*' = .1, '**' = .05, '***' = .01),
         coef_map = c('lprbarr' = 'Log prob. arr.',
                         'lprbconv' = 'Log prob. conv.',
                         'lprbpris'=" Log prob. pris.",
                         'lavgsen'='Log avg. sen.',
                      ## Chicos, aun quiero ver el coeficiente de urban, west y central. 
                      ## Alguno sabe por que no salen los coeficientes?
                      ## Pista: No es un error de programacion, piensen en la version within de estas variables. 
                      'urban'="Urban",
                      'west'="West",
                      'central'="Central"),
         gof_map= modelsummary::gof_map %>%
           filter(raw=="nobs"|raw=="r.squared"| raw=="within.r.squared"))  


