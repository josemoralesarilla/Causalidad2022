library(tidyverse)
library(fixest)
library(haven)
library(stats)
library(dagitty)
library(lavaan)

# http://dagitty.net/primer/

# DAG con problemas
g1 <- dagitty('dag {
    X [pos="2,1"]
    U [pos="2.5,1.05"]
    Y [pos="3,1"]
    
    X -> Y
    X <- U -> Y
}')
plot(g1)

# DAG bajo RDD
g2 <- dagitty('dag {
    VE [pos="0,1"]
    CORTE [pos="1,1"]
    X [pos="2,1"]
    U [pos="2.5,1.05"]
    Y [pos="3,1"]
    
    VE -> CORTE -> X -> Y
    X <- U -> Y
}')
plot(g2)

# EDUC
base <- dagitty('dag {
    "Nota CNU" [pos="0,1"]
    "Universidad" [pos="2,1"]
    "Habilidad" [pos="1.5,-1.05"]
    "Ingresos" [pos="3,1"]
    
    "Nota CNU" -> "Universidad" -> "Ingresos"
    "Nota CNU" <- "Habilidad" -> "Ingresos"
}')
plot(base)

limit <- dagitty('dag {
    "Nota CNU" [pos="0,1"]
    "Superior a D" [pos="1,1.5"]
    "Universidad" [pos="2,1"]
    "Habilidad" [pos="1.5,-1.05"]
    "Ingresos" [pos="3,1"]
    
    "Nota CNU" -> "Superior a D" -> "Universidad" -> "Ingresos"
    "Nota CNU" <- "Habilidad" -> "Ingresos"
}')
plot(limit)

# Data simulada:
set.seed(5)
df <- tibble(
  Habilidad = runif(10000)) %>%
  mutate(
    CNU = (Habilidad + 0.1 * runif(10000)) * 100,
    VE = CNU - 75,
    Pasa = if_else(CNU >= 75, 1, 0),
    Univ = Pasa,
    Ingreso = runif(10000) * 100000 + 20000 * Univ + 20000 * Habilidad)

feols(Ingreso ~ Univ, df)

# Banda de 0.5 puntos alrededor de la discontinuidad.
# Nos quedamos con aquellos que sacaron entre 74.5 y 75.5
banda <- .5
df_banda <- df %>% 
  filter(abs(CNU - 75) < banda)
feols(Ingreso ~ Univ, df_banda)

# Why does this help?
# Habilidad mucho mayor en los que van a la universidad en la data gruesa
ggplot(df, aes(x = Habilidad)) + 
  geom_histogram(data = df %>% filter(Univ == 1),
                 fill = "red", alpha = 0.2,
                 aes(y = stat(density*width))) +
  geom_histogram(data = df %>% filter(Univ == 0),
                 fill = "blue", alpha = 0.2,
                 aes(y = stat(density*width))) +
  labs(y = "Proporción")

# No hay diferencia detectable en habilidad alrededor de la discontinuidad
ggplot(df_banda, aes(x = Habilidad)) + 
  geom_histogram(data = df_banda %>% filter(Univ == 1),
                 fill = "red", alpha = 0.2,
                 aes(y = stat(density*width))) +
  geom_histogram(data = df_banda %>% filter(Univ == 0),
                 fill = "blue", alpha = 0.2,
                 aes(y = stat(density*width))) +
  labs(y = "Proporción")

# Otra forma de verlo:
feols(Habilidad ~ Univ, df)
feols(Habilidad ~ Univ, df_banda)

# Voto y RSE
RSE <- dagitty('dag {
    "Voto" [pos="0,1"]
    "Superior a 50%" [pos="1,1.5"]
    "RSE" [pos="2,1"]
    "U" [pos="1.5,-1.05"]
    "Ganancias" [pos="3,1"]
    
    "Voto" -> "Superior a 50%" -> "RSE" -> "Ganancias"
    "Voto" <- "U" -> "Ganancias"
}')
plot(RSE)
