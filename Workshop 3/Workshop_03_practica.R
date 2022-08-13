## ----setup, include=FALSE----------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, warning = FALSE, error = FALSE, message=FALSE)


## ----------------------------------------------------------------------------------------
# Cargamos las librerias que usaremos hoy
library(dplyr)
library(tidyr)
library(ggplot2)


## ----datos-------------------------------------------------------------------------------
# esta funcion busca datos en las librerias que tengamos abiertas en la sesion,
# los guarda como objetos en nuestro ambiente de trabajo, no hace falta <- esta vez
data("STAR", package = "AER")


# Hechemos un vistazo
head(STAR)



## ----pivot_longer------------------------------------------------------------------------
# La estructura es mas compleja que lo que vimos en la clase anterior
# tuve que crear una rutina para hacer el pivot, pero fue una buena excusa para 
# introducirlos al mundo de las funciones y los loops.

# Esta es una funcion creada por mi. 
# se llama pivot_star_data()
# necesita ser aplicada sobre un data.frame o un tibble que insertas en el argumento "data"
# necesita que se le indique el prefijo de la variable que va a "trasponer"
# nota que no es nada general, solo sirve para resolver esto.

pivot_star_data<-function(data=STAR, 
                          var_name="star"){

data %>% 
  # crea una columna de IDs unicos de cada combinacion
  mutate(id=1:nrow(data)) %>% 
  # selecciona los datos ID de la data + las variables a trasponer
  select(id,gender:birth, starts_with(var_name)) %>% 
  # traspon las variables 
  pivot_longer(cols = !id:birth,  # todas menos las id
               names_to = c("grade"), # los nombres del grado (k,1 ,2 ,3 ) iran a 1 columna
               names_prefix=var_name, # asi le decimos que el nombre del grado debe ser 2, en vez de "star2", por ejemplo 
               values_to=var_name)  
}

# mira como funciona con dos ejemplos
pivot_star_data(data=STAR,var_name = "star")
pivot_star_data(data=STAR,var_name = "read")

# son 8 variables mas, asi que hagamos algo mas practico:
# creemos un loop que repita esto para todas las variables y las junte


# Mi punto de partida es la primera transposicion,
# programo un loop que repita lo mismo para las otras
# variables y las vaya adjuntando una por una a este objeto

STAR_long<-pivot_star_data(data=STAR,var_name = "star")
  
# este es el loop. (ira por todas las variables excepto la primera)

for (variable in c("read","math","lunch","schoolid","degree",
                   "ladder","experience","tethnicity","system")) {
  
  print(paste("estamos transponiendo la variable",variable))
  
  STAR_long<-STAR_long %>% 
    # usamos cbind() esto solo lo puedes hacer cuando tienes 100% certeza que el orden de las
    # filas no ha cambiado. No les recomiendo usarlo.
    # las operaciones join_etc que vimos la clase pasada son mucho mas seguras
    # porque hacen la union en base a una columna de referencia
    # ya saben, ojo con cbind().
    cbind(pivot_star_data(data=STAR,var_name = variable) %>% 
            select(-gender, -ethnicity, -birth,     -grade,-id))
    
}

# prueba de seguridad: El estudio decia que tenia 11600 estudiantes
# el numero de filas / 4 debe ser 11600
nrow(STAR_long)/4 # perfecto



## ----attrition_check---------------------------------------------------------------------
# un primer vistazo levanta alarmas 
# el individuo numero 1 llego a la muestra en 3er grado. Es justo que compare con los que estan desde el comienzo del experimento?

head(STAR_long)

### Contemos la cantidad de individuos que cuentan con menos de 4 registros en la data
in_out_data<-STAR_long %>% 
  # agrupo por individuo y calculo el No. total de periodos que estuvo ausente
  group_by(id) %>% 
  summarise(count=sum(is.na(star))) %>% 
  filter(count>0) 

# 8513 o 18% de los individuos en los datos no estuvo presente durante todo el experimento
in_out_data%>% 
  nrow() 
nrow(in_out_data)/nrow(STAR_long) 

# De estos, el 50% de estos estuvo en 3 periodos, el 29% en 2, y el 20% en 1. 
# El grupo de 1 sola aparicion es el mas peligroso y representa 0.20*0.18 = 0.036 (3.6%) de la data
table(in_out_data$count)

table(in_out_data$count)/nrow(in_out_data)


## ----remove_attrition--------------------------------------------------------------------
### Usemos los comandos join para excluir a los individuos que aparecen en la lista de registros incompletos

STAR_long_2<-STAR_long %>% 
  # anti_join es la que se encarga de esto
  # se especifica igual que las demas. 
  # conserva las observaciones de la data A (left) que no coinciden con la data B (rigth)
  dplyr::anti_join(in_out_data,by="id")


head(STAR_long_2)

# No la vamos usar mas, removamosla.
rm(STAR_long_2)


## ----check_range_score-------------------------------------------------------------------
# Vemos que en teoria ambos indicadores estan en una escala del 1 al 800
# pero en la practica estos rangos son distintos en cada grado, por eso hay que normalizarlo
# Normalizarlo nos dara resultados mas comparables

STAR_long %>% 
  group_by(grade) %>% 
  select(read) %>% 
  skimr::skim()
  

STAR_long %>% 
  group_by(grade) %>% 
  select(math) %>% 
  skimr::skim()



## ----normalize_scores--------------------------------------------------------------------
# Normalicemoslos entre el 0 y 1 a nivel de grado, siguiendo una cdf (funcion de prob acumulada)
# tras esto un valor de 0.6 significara que ese alumno esta por encima del 60% de los alumnos de su grado
STAR_long<-STAR_long %>% 
  group_by(grade) %>% 
  mutate(read_cdf=percent_rank(read),
         math_cdf=percent_rank(math)) %>% 
  mutate(score=math_cdf*0.5+read_cdf*0.5,
         score_raw=math*0.5+read*0.5) 




## ----density_plot_facet------------------------------------------------------------------
# make a density plot
ggplot(data = STAR_long, 
       mapping = aes(x = score,
                     color=factor(star,
                                  levels = c("regular",
                                             "regular+aide",
                                             "small")))) + 
  geom_density() + 
  # Este comando te permite segmentar un grafico de ggplot sobre segmentos de la data
  # creando un panel de graficos para cada uno de estos
  facet_wrap(~ factor(paste0("Star ",grade),
                      levels = c("Star k","Star 1",
                                 "Star 2","Star 3"))) +
  labs(color="Class size")

## ----boxplot_facet-----------------------------------------------------------------------
# make a boxplot
ggplot(data = drop_na(STAR_long), 
       mapping = aes(x = factor(star,
                                  levels = c("regular",
                                             "regular+aide",
                                             "small")),
                     y=score)) + 
  geom_boxplot() + 
  facet_wrap(~ factor(paste0("Star ",grade),
                      levels = c("Star k","Star 1",
                                 "Star 2","Star 3"))) +
  labs(color="Class size",
       x="Tratamiento (tamano de clase)")


## ----results_hme, echo=FALSE-------------------------------------------------------------
library(knitr)
library(kableExtra)
library(gt)
# Tabla de estadisticos descriptivos
tab221_data <- data.frame(
  v1 = c("Free lunch", "White/Asian", "Age in 1985", "Attrition rate",
    "K. class size", "K. test percentile"),
  v2 = c(0.47, 0.68, 5.44, 0.49, 15.10, 54.70),
  v3 = c(0.48, 0.67, 5.43, 0.52, 22.40, 48.90),
  v4 = c(0.50, 0.66, 5.42, 0.53, 22.80, 50.00),
  v5 = c(0.09, 0.26, 0.32, 0.02, 0.00, 0.00)
) 

# Usamos el paquete gt:: para imprimir tablas en formato de publicacion
# Revisenlo, es muy flexible para presentar resumenes de datos.
# No es tan bueno para imprimir resultados de regresiones, para eso les recomiendo huxtable::
tab221_data%>%
  gt() %>%
  gt::tab_header("Tabla 2.2.1, MHE ",
                 subtitle = "Valores directamente extraidos de la fuente") %>%
  gt::cols_label(v1="Variable",v2= "Small",
                 v3="Regular",v4= "Regular + Aide",v5= "P-value")





## ----replicate_mhe_1---------------------------------------------------------------------
# Intento de replicar la tabla 2.2.1 de MHE
# All variables are for the first year a student is observed
# Here we filter the table to fulfill that 
tabla_stats<-STAR_long %>% 
  group_by(id) %>% 
  mutate(grade_num=as.numeric(ifelse(grade=="k","0",grade))) %>%
  filter(grade_num==min(grade_num)) %>% 
  # recuerda aplicar ungroup despues de usar group_by: mejora velocidad y previene errores
  ungroup() %>% 
  # one of the rows reffers to white/assian, so we create a variable that blends these groups
  # carefull about the factor vs character format, it can cause errors 
  mutate(ethnicity=as.character(ethnicity),
         table_etnicity=ifelse(ethnicity %in% c("cauc","asian"),
                               "white/asian",ethnicity)) %>% 
  # Another variable we need to create is age in 1985
  mutate(age=1985-birth)

#  The P-value in the last column is for the F-test of equality of variable means across all three groups. 
# This functions pulls the joint p value from a regression object
lmp <- function (modelobject) {
  # le anades un modelo lm. Ej: lm(precio~cantidad, data=mercado)
  # si no es un objeto lm, te arrojara un mensaje de error
    if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
    # extraemos el elemento fstatistic de esos resultados
    f <- summary(modelobject)$fstatistic
    # con el f, y los grados de libertad, extraemos el joint p-value
    p <- pf(f[1],f[2],f[3],lower.tail=F)
    # limpiamos el joint p-value para que sea un numero "plano": sin atributos
    attributes(p) <- NULL
    return(p)
}

## En clase lo haremos para algunas variables:
# free_lunch (clase)
# white/asian (clase)
# age in 1985 (clase)
# Attrition rate (no lo haremos, pero revisen en MHE que se trata y repasen implicaciones)
# k. class size (no lo haremos, pero revisen en MHE que se trata y repasen implicaciones)
# k. test percentile (clase)


## ----replicate_mhe_2, echo=FALSE, purl=TRUE----------------------------------------------
free_data<-tabla_stats%>% 
  group_by(star,lunch) %>% 
  summarise(count=n()) %>% 
  group_by(star) %>% 
  mutate(share=count/sum(count)) %>% 
  filter(lunch=="free") %>% 
  select(-count) %>% 
  tidyr::pivot_wider(names_from = star, values_from=share) %>% 
  mutate(p_value=lmp(lm(lunch=="free"~star,data=tabla_stats )))%>% 
  rename(variable=1) %>% 
  mutate(variable="free lunch")

white_asian_data<-tabla_stats %>% 
  group_by(star,table_etnicity) %>% 
  summarise(count=n()) %>% 
  group_by(star) %>% 
  mutate(share=count/sum(count)) %>% 
  filter(table_etnicity %in% c("white/asian")) %>% 
  select(-count) %>% 
  tidyr::pivot_wider(names_from = star, values_from=share) %>% 
  mutate(p_value=lmp(lm(table_etnicity=="white/asian"~star,data=tabla_stats ))) %>% 
  rename(variable=1)


age_data<-tabla_stats %>% 
  # esta no da igual, debe ser porque estoy calculando edad 
  # de manera incorrecta. Hay bono para quien pueda corregirlo.
  group_by(star) %>% 
  summarise(age=mean(age, na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = star, values_from=age) %>% 
  mutate(p_value=lmp(lm(age~star,data=tabla_stats ))) %>% 
  mutate(variable="Edad")  
  



## ----replicate_mhe_3---------------------------------------------------------------------
kinder_table<-tabla_stats %>% 
  filter(grade=="k")

score_data<-kinder_table%>% 
  group_by(star) %>% 
  summarise(score=mean(score, na.rm = T)) %>% 
  tidyr::pivot_wider(names_from = star, values_from=score) %>% 
  mutate(p_value=lmp(lm(score~star,data=tabla_stats )))%>% 
  mutate(variable="Score promedio en Kinder")  

dplyr::bind_rows(free_data,
                 white_asian_data,
                 age_data,
                 tab221_data %>% 
                  filter(v1=="Attrition rate"|v1=="K. class size") %>% 
                   rename(variable=v1, small=v2,regular=v3,`regular+aide`=v4,p_value=v5),
                 score_data) %>% 
  select(variable, small,regular,`regular+aide`,p_value) %>% 
  # Usamos el paquete gt:: para imprimir tablas en formato de publicacion
  # Revisenlo, es muy flexible para presentar resumenes de datos.
  # No es tan bueno para imprimir resultados de regresiones, para eso les recomiendo huxtable::
  gt() %>% 
  gt::tab_header("Tabla 2.2.1, MHE ",
                 subtitle = "Adaptada usando datos de libreria AER::STARS") %>% 
  gt::fmt_number(columns = 2:5)




## ----regresion_mhe-----------------------------------------------------------------------
# Creemos las variables dummys de forma practica
kinder_table =  as_tibble(kinder_table) %>%
    mutate(girl  = gender == "female",
           freelunch = lunch == "free")

library(huxtable)
lm1<-lm(score ~ star , data=kinder_table )
lm2<-lm(score ~ star + schoolid, data = kinder_table)
lm3<-lm(score ~ star + schoolid + girl + lunch, data = kinder_table)

# la variable schoolid toma decenas de valores, tenemos que remover esos coeficientes de nuestra tabla. Usaremos el siguiente codigo.

# Extraemos el nombre de todos los coeficientes y extraemos los que empiezan con "schoolid"
school_co = grep(names(coef(lm2)),pattern = "schoolid*",value=T)
# nos aseguremos de incluir todos los schoolids en las regresiones
school_co = c(unique(school_co,grep(names(coef(lm2)),pattern = "schoolid*",value=T)),"schoolid77")

huxreg(lm1, lm2, lm3, 
       omit_coefs = school_co,  
       statistics = c("N. obs." = "nobs", 
      "R squared" = "r.squared", "F statistic" = "statistic",
      "P value" = "p.value")) %>% 
    huxtable::insert_row(c("School control (FE)","No","Yes","Yes"),after = 11) 

## Para tarea, replicar tabla de regresion e interpretar resultados.


