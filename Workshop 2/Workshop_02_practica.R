# HEADER --------------------------------------------
#
# UNIVERSIDAD CATOLICA ANDRES BELLO
# Class: INTRODUCCION A LA INFERENCIA CAUSAL
#
# Authors: Carlos J. Daboin
# Email:  cdaboin2@gmail.com
# 
# Date: 2022-05-01
# 
# Alumno: _______________
#
# Notes: Trateremos de completarlo en clase pero no se preocupen, les enviare uno resuleto despues.

# Librerias---------------------------------------
librerias <- c("dplyr","tidyr","ggplot2",
               "skimr","nycflights13","janitor",
               "knitr","rmarkdown")

# Instalar librerias aun no instaladas
installed_packages <- librerias %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(librerias[!installed_packages])
}

library(readr)        # Libreria para leer los datos
library(tidyverse)    # ya saben para que: aca estan dplry, tidyr, ggplot y otros paquetes utiles
library(nycflights13) # base de datos para practicar

# Data--------------------------------------------------
WDI_wide<-read_csv("data/WDI_extract_data.csv")


# Revision de datos------------------------------------
## Vistazo a primeras filas
### head() muestra por default las primeras 10
WDI_wide %>% head() 

## Vistazo a ultimas filas
### tail() muestra por default las ulltimas 10
WDI_wide %>% tail()


# Transformacion de datos---------------------------------
## Filtrado preliminar: Conserva solo lo que te interesa para no complicarnos de mas ---------------------------------
### Nota: Recuerda que los dos puntos despues del nombre de una libreria sirven para invocar a sus funciones. Usamos este formato para que veas claramente de donde sale cada funcion, pero no es necesario.

WDI_long<-WDI_wide %>% 
  # janitor::clean_names() remueve espacios y mayusculas del nombre de las columnas
  janitor::clean_names() %>% 
  # Siempre usaremos el paquete dplry de la clase pasada
  # Remover columnas que no van a necesitar
  # Remover registros problematicos
  dplyr::select(-series_code) %>% 
  dplyr::filter(! (is.na(country_code) | country_name=="World")) 

head(WDI_long)


## Empezemos a poner la data en formato tidy ----------------------------------------------------------

### Primero pongamos los años en una sola columna --------------------------------------------------------------

#### Ejemplo Pivot Longer
##### Ejemplo con datos de los billboards
##### Cada semana esta en una columna, y los valores son la cantidad de reproducciones por semana
head(billboard)

tidyr::billboard %>%
  tidyr::pivot_longer(
    cols = starts_with("wk"), # columnas para transponer en formato long
    names_to = "week",        # string con el nombre de la columna que contendra los nombres de las columnas
    names_prefix = "wk",      # expresion regular (investigen esto) que remueve la parte de los nombres que no nos guste
    values_to = "rank",       # columna nueva que tendra los valores de las columnas transpuestas 
    values_drop_na = TRUE     # muchas celdas no tenian valores. Escoge si los conservas o los remueves
  )

#### De vuelta a nuestra tarea 
WDI_long<-WDI_long %>%
  # Cada variable debe tener su propia columna
  # 1. Unifica las columnas de año.
  # tidyr::_____ trasnpone K columnas en 2 columnas:
  # una con los nombres de las antiguas columnas, y otra con los valores
  tidyr::______(cols = starts_with("x"),
               names_to = "year",
               values_to="value"  )

## Como hago para ver como quedo?
____(WDI_long)

### Aun falta: Hay que poner cada indicador en su propia columna --------------------------------------------------------------

#### Ejemplo pivot wider
#### Ejemplo con datos de los us_rent_income
##### Los estimados y los margenes de error estan la misma columna
head(us_rent_income)

# Transforma la data generando una columna para los estimados y otra para los margenes de error
us_rent_income %>%
  pivot_wider(names_from = variable,         # La variable con las categorias que vas a individualizar 
              values_from = c(estimate, moe) # La(s) variable(s) que tienen los valores a repartir entre las nuevas columnas
  ) 

#### De vuelta a nuestra tarea
WDI_long<-WDI_long %>%
 # 2. Asigna una columna nueva a cada indicador
  # tidyr::pivot_____() selecciona los valores en una columna y los usa para
  # nombrar nuevas variables. Luego rellena estas columnas con los valores que le indiques
  tidyr::_____(names_from = c("series_name"),
              values_from= value)

### Corrijamos detalles menores de nombres y formato -------------------------------------------------------
WDI_long<-WDI_long %>%
  # 3. Put more code-firendly names (These are too long, and have spaces)
  dplyr::______(gdp_pc="GDP per capita (current US$)",
         population="Population, total",
         gdp="GDP (current US$)",
         life_exp="Life expectancy at birth, total (years)") %>%
  # 4. change format to numeric
  dplyr::mutate_at(vars(gdp, gdp_pc,life_exp, population), as.numeric) %>%
  # 5. fix the years column: x1961_yr1961 get last 4 characters
  dplyr::______(year=substr(year,
                            stringr::str_length(year)-3,
                            stringr::str_length(year))) %>%
  # 6. Remove year 2020 since it has too many missing estimates to date
  dplyr::______(year=as.numeric(year)) %>%
  dplyr::______(year<2020)


### Finalmente, junta los nombres de cada continente  (join) ------------------------------------------------

#### Ejemplo con los datos de aerolineas 
## Revisemos los datos como siempre
____(flights)
____(airlines)

#### Que aerolineas tuvieron los mayores retrasos en las llegadas?
#### 
#### 1. Join `airlines` to `flights`
#### 2. Compute and order the average arrival delays by airline. Display full names, no codes.

flights %>%
  dplyr::drop_na(arr_delay) %>%
  dplyr::left_join(airlines, by="carrier") %>%
  dplyr::group_by(name) %>%
  dplyr::______(mean_arrival_delays=mean(arr_delay)) %>%
  dplyr::arrange(_____)


#### De vuelta a nuestra tarea
##### Carlos creó el archivo "data/country-and-continent-codes-list.csv" en excel para  que tengamos datos de continentes
##### No es que le guste, pero cuando toca, toca.
##### Lee los datos 
continents<-readr::read_delim("data/country-and-continent-codes-list.csv") 

continents<-continents %>%
  # Les presento janitor::clean_names(), muy util para limpiar nombres de manera consistente
  janitor::clean_names() %>%
  dplyr::_______(continent_name ,three_letter_country_code)

WDI_long<-WDI_long %>%
  # 7. We need continent names: Join the continent names data.
  # there are registers for groups of countries, you need an inner join
  # there are other kinds of join: left, right, full
  dplyr::_______(continents,
             by=c("country_code"="three_letter_country_code"))


## Echa un vistazo a la data: que piensas-------------------------------------------------

______(WDI_long)
______(WDI_long)

### Solo hace falta que filtremos la data y nos quedemos con un año: 2007
WDI_long<-WDI_long %>%
  dplyr::______(year==2007)



## Graficos de distribucion--------------------------------------------------------------
WDI_long_07 %>%
  _____(aes(y=life_exp,x=continent_name))+#<<
  # Geom de distribucion por cuartiles (boxplot), por grupos
  ______(aes(fill=continent_name), alpha=0.4) +
  # Anade puntos en el fondo
  geom_____(aes(fill=continent_name),alpha=0.8, show.legend = FALSE) +
  # Geom de lineas verticales
  geom__(aes(yintercept = mean(life_exp),linetype="Mean"))+
  labs(title="Distribución de esperanza de vida por continente",
       fill="Muestra",
       linetype="Stats",
       x=NULL,
       y="Esperanza de vida")

WDI_long_07 %>%
  _____(____(y=gdp_pc,x=continent_name))+
  # Geom de distribucion por cuartiles (boxplot), por grupos
  _____(aes(fill=continent_name), alpha=0.4) +
  # Anade puntos en el fondo
  ______(aes(color=continent_name),alpha=0.8, show.legend = FALSE) +
  # Geom de lineas verticales
  ______(aes(yintercept = mean(____,na.rm = T),linetype="Mean"))+
  # Give log scale to Y axis
  scale_y_log10(labels=scales::number_format())+
  labs(title="Distribución del ingreso per capita por continente",
       fill="Muestra",
       linetype="Stats",
       x=NULL,
       y="GDP Per Capita")


## Scatter plots--------------------------------------------------------------

### Una grafica basica
ggplot(data=WDI_long_07,
       mapping = ________(y=life_exp,x=gdp_pc))+
  # coordenadas para una geometria especifica
  ________()+
  scale_size_continuous(labels=scales::number_format())+
  scale_x_log10()+
  labs(title="Relacion entre PIB Per Capita y esperanza de vida",
       x="Gdp Per Capita (log scale)",
       y="Life expectancy",
       size="Pop (millions)")

### Anademos detalle sobre la poblacion y el detalle
ggplot(data=WDI_long_07,
       mapping = aes(y=life_exp,x=gdp_pc))+
  # coordenadas para una geometria especifica
  ______(aes(_____=population/1000000,
                 color=_______))+
  scale_size_continuous(labels=scales::number_format())+
  scale_x_log10()+
  labs(x="Gdp Per Capita (log scale)",
       y="Life expectancy",
       size="Pop (millions)")


## Muchos de los lectores del diario no perciben los colores, asi que nos sugieren diferenciar los continentes con la forma de los puntos.
ggplot(data=WDI_long_07,
       mapping = aes(y=life_exp,x=gdp_pc))+
  # coordenadas para una geometria especifica
  _____(aes(size=population/1000000,
                 ____=continent_name))+
  scale_size_continuous(labels=scales::number_format())+
  scale_x_log10()+
  labs(title="Relacion entre PIB Per Capita y esperanza de vida",
       x="Gdp Per Capita (log scale)",
       y="Life expectancy",
       size="Pop (millions)")
