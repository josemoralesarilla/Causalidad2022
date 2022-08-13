# Workshop 1 - Raw code


# Preparativos ------------------------------------------------------------

# Carga librerias (asegurate de haberlas instalado)
library(tidyverse) # varias librerias manipulacion de datos
library(causaldata)# Distintos data sets para jugar

#Guardo datos de libreria causal  (filtrados)
gapminder_07<-gapminder %>% 
  filter(year==2007)


# leamos los datos extraidos del WB
WDI_wide<-read_csv("data/WDI_extract_data.csv")

#leamos una lista de nombre de continentes por pais que yo cree
continents<-read_csv("data/country-and-continent-codes-list.csv") %>% 
  janitor::clean_names() %>% 
  select(continent_name ,three_letter_country_code)

# Cambiemos la forma de la base de datos 
## Aprenderemos a hacer esto en la Clase II, no se preocupen
WDI_long<-WDI_wide %>% 
  janitor::clean_names() %>% 
  # remuevan columnas que no van a necesitar
  select(-series_code) %>% 
  # Remover registros problematicos
  filter(! (is.na(country_code) | country_name=="World")) %>% 
  # Cada variable debe tener su propia columna
  # 1. Unifica la data por anho
  pivot_longer(cols = starts_with("x"),
               names_to = "year",
               values_to="value"  ) %>%
  # 2. Desagrega los indicadores por columna
  pivot_wider(names_from = c("series_name"),
              values_from= value) %>% 
  # 3. Put more code-firendly names
  rename(gdp_pc="GDP per capita (current US$)",
         population="Population, total",
         gdp="GDP (current US$)",
         life_exp="Life expectancy at birth, total (years)") %>% 
  # 4. change format to numeric
  mutate_at(vars(gdp, gdp_pc,life_exp, population), as.numeric) %>% 
  # 5. fix the years column: x1961_yr1961 get last 4 characters
  mutate(year=substr(year,str_length(year)-3,str_length(year))) %>% 
  mutate(year=as.numeric(year))

WDI_long<-WDI_long %>% 
  # 6. Join the continent names to 
  # there are registers for groups of countries, you need an inner join
  inner_join(continents, by=c("country_code"="three_letter_country_code")) %>% 
  # 7. Remove year 2020 since it has too many missing estimates to date
  filter(year<2020)

## La libreria janitor (instalenla) tiene una funcion para limpiar nombres
WDI_wide %>% 
  janitor::clean_names() 

WDI_wide %>% 
  janitor::clean_names()%>% 
  tail()



#. Objetios y funciones -------------------------------------------------

## revisa la documentación
??matrix()
# data: opcional, require insertar un vector
# nrow: número filas de la matriz
# ncol: número columnas de la matriz


# Creemos un vector con un cero
obj_1<-0

# Matriz A: 5x2 llena de ceros
A<-matrix(data = obj_1, nrow = 5, ncol = 2)

# Veamos la matriz A
A

# Creemos un vector con numeros del 1 al 10
obj_2<-c(1:10)

# Matrix B: 5x2 con una sequencia numerica
B<-matrix( data = obj_2, nrow = 5, ncol = 2)

# Veamos la matriz B
B


# # Funciones para análisis estadístico -----------------------------------

x<-c(1:10) # vector x
y<-x*2+5   # vector y
# Mean
mean(x)
# Median
median(x)
# Std. dev. and variance
sd(x)
var(x)
# Min. and max.
min(x)
max(x)
# Correlation/covariance
cor(x, y)
cov(x, y)
# Quartiles and mean of x
summary(x)


# Set seed (pin down random number generation)
set.seed(1)
# 4 random draws from N(3,5)
rnorm(n = 4, mean = 3, sd = sqrt(5))
# CDF for N(0,1) at z=1.96
pnorm(q = 1.96, mean = 0, sd = 1)
# Sample 5 draws from x w/ repl.
sample(
  x = x,
  size = 5,
  replace = T
)
# First and last 3 elements of x
head(x, 3)
tail(x, 3)


# Set seed (pin down random number generation)
set.seed(1)
# 4 random draws from N(3,5)
distribucion_normal<-rnorm(n = 4, mean = 3, sd = sqrt(5))
# CDF for N(0,1) at z=1.96
cdf<-pnorm(q = 1.96, mean = 0, sd = 1)
# Sample 5 draws from x w/ repl.
muestra<-sample(
  x = x,
  size = 5,
  replace = T
)
# First and last 3 elements of x
head_x<-head(x, 3)
tail_x<-tail(x, 3)


# Otras funciones
# Set seed (pin down random number generation)
set.seed(1)
# 4 random draws from N(3,5)
distribucion_normal<-rnorm(n = 4, mean = 3, sd = sqrt(5))
# CDF for N(0,1) at z=1.96
cdf<-pnorm(q = 1.96, mean = 0, sd = 1)
# Sample 5 draws from x w/ repl.
muestra<-sample(
  x = x,
  size = 5,
  replace = T
)
# First and last 3 elements of x
head_x<-head(x, 3)
tail_x<-tail(x, 3)

#  Introducción al tidyverse ----------------------------------------------

# Ver datos con kable (con formato HTML para presentaciones y sitios web)

knitr::kable(WDI_wide[1:4,] %>% 
               select("Country Name","Series Name","Series Code",
                      "1960 [YR1960]", "1961 [YR1961]", "1962 [YR1962]"),
             format = 'html')


knitr::kable(WDI_long[1:8,] %>% 
               select("year","continent_name","country_name",
                      "gdp_pc","life_exp", "population"),
             format = 'html')

# dplyr en acción -------------------------

## 
### Creamos data.frame terrible
datos_terribles<-data.frame(
    nombre_terrible.1=c(rep("A",2),rep("B",2)),
    value=round(rnorm(4,mean = 0,sd=1),2),
    nombre.peor.2=c(rep("I",2),rep("II",2))  )
### Lo inspeccionamos
head(datos_terribles)



# Usa **select()** para darle orden a tus datos
# Reordena las columnas 
datos_terribles %>% 
      select(nombre_terrible.1, nombre.peor.2,value) 

# Reordena, excluye y cambia los nombres: 
    datos_terribles %>% 
      select(categoria=nombre_terrible.1,
             valor=value)
# Selecciona variables según su formato 
datos_terribles %>% 
      select(where(is.numeric)) 

# Selecciona variables según su nombre 
datos_terribles %>% 
      select(starts_with("nombre"))

# Restringe tu analisis a grupos específicos 
# Con   **filter()**

  WDI_long %>% 
    ## only years where data is available
    filter(year==2018) %>% 
    ## only North American countries
    filter(continent_name=="South America" ) %>% 
    ## only year, country, and per capita gdp
    select(year,country_name,gdp_pc)


# Crea nuevas variables 
# Con **mutate()**

  WDI_long %>% 
    ## select variables of your interest
    select(year,country_name,gdp_pc ,population) %>% 
    ## estimate total GDP by country (in billions). Keep 3 decimals
    mutate(gdp_bn=round(population*gdp_pc/(10^9),3)) %>% 
    head()
# Obten montos totales, promedios y otras medidas agregadas
# Con  **summarise()**
    
  WDI_long %>% 
    ## keep year 2018
    filter(year==2018) %>% 
    ## estimate total GDP by country (in billions). Keep 3 decimals
    mutate(gdp_bn=round(population*gdp_pc/(10^9),3)) %>% 
    ## estimate world gdp and world population
    summarise(life_exp=mean(life_exp, na.rm = T),
              population=sum(population, na.rm = T),
              gdp_bn=sum(gdp_bn, na.rm = T))

#Extiende tus cálculos a lo largo de diferentes grupos
# Con  **group_by()**
    
  WDI_long %>%
    filter(year==2018) %>% 
    group_by(continent_name) %>% 
    summarise(count=n(),
              mean_life_exp=mean(life_exp,na.rm = T),
              sd_life_exp=sd(life_exp,na.rm = T))
  
  

# Dplry en accion, desaafios  ---------------------------------------------
  # **Datos:** Ingreso per cápita de cada país desde 1950 hasta 2018. 
  # 
  # **Desafío:** Obtén un resumen de la distribucion del ingreso por continente en el año mas reciente ¿cúal sintáxis te parece mas clara?
    
    
  ### (a) Con funciones base de R:
  
 aggregate(x = WDI_long[WDI_long$year==max(WDI_long$year),"gdp_pc"]  , 
            list(continent = WDI_long[WDI_long$year==max(WDI_long$year),]$continent_name), 
            FUN = function(x) c(min=min(x,na.rm = T),
                                mean=mean(x,na.rm = T),
                                max=max(x,na.rm = T))) %>% as.tibble()
  ### (b) Con dplyr:
  
  WDI_long %>%
    filter(year==max(year)) %>% 
    group_by(continent_name) %>% 
    summarize(min=min(gdp_pc,na.rm = T),
              mean=mean(gdp_pc,na.rm = T),
              max=max(gdp_pc,na.rm = T))
  
  # **Desafío:** ¿Qué es mayor? ¿La varianza del ingreso entre continentes o la varianza lo interno de cada continente? 
  # Replica el F-statistic del analisis de varianzas (Test ANOVA):
  # 1. Calcula la variancia entre grupos ("between"):    
  #   1.1 Calcula el promedio de cada grupo    
  #   1.2 Calcula la variance entre las medias muestrales y la media total
  # 2. Calcula la varianza a lo interno de cada grupo ("within")    
  # 
  # 3. Produce el F-statistic: retio entre  Varbetween/Varwithin
  
  between_variance<-gapminder_07 %>% # Data similar a WDI
    ## Calcula la media total  
    mutate(total_mean=mean(lifeExp)) %>% 
    ## Calcula la media por continente con summarise
    group_by(continent) %>% 
    summarise(n_countries=n(),
              group_mean=mean(lifeExp),
              total_mean=mean(total_mean)) %>% 
    ungroup() %>% 
    ## Calcula las diferencias between
    mutate(dif_btw=(group_mean-total_mean)) %>% 
    ## calcula la varianza entre continentes
    summarise(var_btw=sum(n_countries*dif_btw^2)/(n()-1))
  
  within_variance<-gapminder_07 %>% # Data similar a WDI
    ## Calcula la media por continente con summarise
    group_by(continent) %>% 
    mutate(group_mean=mean(lifeExp)) %>% 
    ungroup() %>% 
    ## Calcula las diferencias within continents
    mutate(dif_wtn=(lifeExp-group_mean)) %>% 
    # Calcula varianzas: 1) Eleva las diferencias al cuadrado, 
    ## 2) sumalas, y divide entre tamano de muestra -df 
    summarise(var_wtn=sum(dif_wtn^2)/(n()-5))
  
  # Usa print() y paste() para dejar mensajes personalizados
  
  message_1<-print(paste("La varianza entre continentes es: ", 
                         round(between_variance$var_btw,1)))
  message_2<-print(paste("La varianza intra-continental es: ", 
                         round(within_variance$var_wtn,1)))
  message_3<-print(paste("El ratio entre ambas (F-statistic) es:", 
                         between_variance$var_btw/within_variance$var_wtn))
  
  message_1
  message_2
  message_3
  
  # Compare it with the output in the analysis of variance command
  aov.model <- aov(lifeExp ~ continent,
                   data = gapminder_07)
  # Summary of the analysis
  summary(aov.model)
  

# Visualizacion de datos --------------------------------------------------
  # preparo los datos
  gdp_pc_by_year<-WDI_long %>% 
    filter(year>1990) %>% 
    group_by(year) %>% 
    summarise(gdp_pc=weighted.mean(gdp_pc,w=population,na.rm = TRUE))
  

## Mapeo vs coordenadas y Geoms --------------------------------------------------
  
  
  ggplot(data = gdp_pc_by_year,
         aes(x = year, y = gdp_pc)) +
    geom_point(aes(color = gdp_pc)) + #<<
    labs(x = "Year",
         y = "GDP per capita")
  
  ggplot(data = gdp_pc_by_year,
         aes(x = year, y = gdp_pc)) +
    geom_line(color = "navy") + #<<
    labs(x = "Year",
         y = "GDP per capita")
  
  
  ggplot(data = gdp_pc_by_year,
         aes(x = year, y = gdp_pc)) +
    geom_line(aes(color = "navy")) + #<<
    labs(x = "Year",
         y = "GDP per capita")
  
  ## Gráficos de distribución
  
  # **Distribución del ingreso por habitante**
    
      # Total
      ggplot(data=gapminder_07, aes(x=gdpPercap))+
        # Geom de distribuciones de densidad. Ponemos color en fill afuera de los aesthetics
        geom_density(fill="gray")+
        # Geom de lineas verticales. Requiren el valor del punto de corte
        # Usamos la palabra "Mean" en punto de corte para que la legenda de la 
        # linea muestre esa palabra (not correct, but effective).
        geom_vline(aes(xintercept = mean(gdpPercap),linetype="Mean"))+
        labs(title="Distribución completa",
             linetype="Stats",
             x="Ingreso por habitante")
      
      # Por continente
      ggplot(data= gapminder_07, aes(x=gdpPercap))+
        # Geom de distribucion de densidades, especificando grupos
        geom_density(aes(fill=continent), alpha=0.4) +
        # Geom de lineas verticales
        geom_vline(aes(xintercept = mean(gdpPercap),linetype="Mean"))+
        # Geom de lineas verticales por contienente. Insertamos datos agregados a nivel continente para trazar varias lineas
        geom_vline(data= group_by(gapminder_07,continent) %>% 
                     summarise(gdpPercap=mean(gdpPercap)),
                   aes(xintercept = gdpPercap,color=continent), 
                   show.legend = F)+
        scale_x_log10()+
        labs(title="Distribución por continente",
             fill="Muestra",
             linetype="Stats",
             x="Ingreso por habitante")
      # **Distribución de la esperanza de vida en el mundo** 
      
      gapminder_07 %>%
        ggplot(aes(x=lifeExp))+
        # Geom de distribuciones de densidad. Ponemos color en fill afuera de los aesthetics
        geom_density(fill="gray")+
        # Geom de lineas verticales. Requiren el valor del punto de corte
        # Usamos la palabra "Mean" en punto de corte para que la legenda de la 
        # linea muestre esa palabra (not correct, but effective).
        geom_vline(aes(xintercept = mean(lifeExp),linetype="Mean"))+
        labs(title="Distribución completa",
             linetype="Stats",
             x="Esperanza de vida")
      
      # **Distribución de la esperanza de vida, por continente** 
      gapminder_07 %>%
        ggplot(aes(x=lifeExp))+
        # Geom de distribucion de densidades, especificando grupos
        geom_density(aes(fill=continent), alpha=0.4) +
        # Geom de lineas verticales
        geom_vline(aes(xintercept = mean(lifeExp),linetype="Mean"))+
        # Geom de lineas verticales por contienente
        # Insertamos datos agregados a nivel continente para trazar varias
        # lineas
        geom_vline(data= gapminder_07 %>% 
                     group_by(continent) %>% 
                     summarise(lifeExp=mean(lifeExp)),
                   aes(xintercept = lifeExp,color=continent), 
                   show.legend = F)+
        scale_x_log10()+
        labs(title="Distribución por continente",
             fill="Muestra",
             linetype="Stats",
             x="Esperanza de vida")
      
      ## Relación entre dos variables continuas
      # **Define la data, las coordenadas, y la forma**
      # Datos y coordenadas
      ggplot(data=gapminder_07, 
             mapping = aes(y=lifeExp,x=gdpPercap))+
        # Formas o geometrias
        geom_point()+
        labs(x="Gdp Per Capita",
             y="Life expectancy")
      # **Añade otras formas y haz cambios en el formato**
      
      ggplot(data=gapminder_07, 
             mapping = aes(y=lifeExp,x=gdpPercap))+
        # coordenadas para una geometria especifica
        geom_point(aes(size=pop/1000000, color=continent))+
        scale_size_continuous(labels=scales::number_format())+
        labs(x="Gdp Per Capita",
             y="Life expectancy",
             size="Pop (millions)")
      
      # **Cambiemos la escala de gpd per capita ¿Qué ganámos con logs?**
      
      ggplot(data=gapminder_07, 
             mapping = aes(y=lifeExp,x=gdpPercap))+
        # coordenadas para una geometria especifica
        geom_point(aes(size=pop/1000000, color=continent))+
        scale_size_continuous(labels=scales::number_format())+
        scale_x_log10()+
        labs(x="Gdp Per Capita (log scale)",
             y="Life expectancy",
             size="Pop (millions)")  
              