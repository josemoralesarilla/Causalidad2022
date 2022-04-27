# Proporción de antimonchismo
library(tidyverse)
tamaño = 1000000 # Un millón de moncholenses
tasa_antimonchista = .51 # Solo Dios sabe que una mayoría quiere salir de Monchulo.
población = tibble(
  index = 1:tamaño,
  antimonchista = (index <= tasa_antimonchista * tamaño),
  ingresos = 15000 * antimonchista + rnorm(tamaño, 1000000, 30000) # Antis $15k más ricos.
)

# Una encuestadora
set.seed(13) # Replicar muestra aleatoria
tamaño_muestra = 1000

muestra <- población %>% 
  sample_n(tamaño_muestra)

prop_anti_muestra <- muestra %>% 
  summarize(mean(antimonchista)) %>%
  as.numeric() 

# Otra encuestadora
muestra_2 <- población %>% 
  sample_n(tamaño_muestra)

prop_anti_muestra_2 <- muestra_2 %>% 
  summarize(mean(antimonchista)) %>%
  as.numeric()

# Otra encuestadora
muestra_3 <- población %>% 
  sample_n(tamaño_muestra)

prop_anti_muestra_3 <- muestra_3 %>% 
  summarize(mean(antimonchista)) %>%
  as.numeric()

# Siete encuestadoras más y promedia
muestras_anti <- c(prop_anti_muestra, 
                   prop_anti_muestra_2, 
                   prop_anti_muestra_3)

for(i in 1:7){
  muestra <- población %>% 
    sample_n(tamaño_muestra)
  
  muestras_anti[i+3] <- muestra %>%
    summarize(mean(antimonchista)) %>%
    as.numeric()
}

promedio_10 <- mean(muestras_anti)

h1 <- ggplot(data = tibble(x = muestras_anti), 
             aes(x = x, y = stat(density*width))) +
  geom_histogram() + 
  labs(title="Histograma 10 encuestas",
       x="Antimonchismo (%)", 
       y = "Proporción de encuestas")

promedio_10

h1

# 500 encuestas...? Aproximando la distribución muestral.
for(i in 1:490){
  muestra <- población %>% 
    sample_n(tamaño_muestra)
  
  muestras_anti[i+10] <- muestra %>%
    summarize(mean(antimonchista)) %>%
    as.numeric()
}

media_dist_muestral <- mean(muestras_anti)
EE_dist_muestral <- sd(muestras_anti)

dist_muestral <- ggplot(data = tibble(x = muestras_anti), 
                        aes(x = x, y = stat(density*width))) +
  geom_histogram() + 
  labs(title="Distribución Muestral",
       x="Antimonchismo (%)", 
       y = "Proporción de encuestas")

dist_muestral

# Distribución muestral de los ingresos
muestras_ing <- c()
for(i in 1:500){
  muestra <- población %>% 
    sample_n(tamaño_muestra)
  
  muestras_ing[i] <- muestra %>%
    summarize(mean(ingresos)) %>%
    as.numeric()
}

media_dist_muestral_ing <- mean(muestras_ing)
EE_dist_muestral_ing <- sd(muestras_ing)

dist_muestral_ing <- ggplot(
  data = tibble(x = muestras_ing), 
  aes(x = x, y = stat(density*width))) +
  geom_histogram() + 
  labs(title="Distribución Muestral",
       x="Ingresos ($)", 
       y = "Proporción de encuestas")

dist_muestral_ing

# Distribución muestral de la correlación entre ingresos y antimonchismo
muestras_corr <- c()
for(i in 1:500){
  muestra <- población %>% 
    sample_n(tamaño_muestra)
  
  modelo <- lm(ingresos ~ antimonchista, muestra)
  
  muestras_corr[i] <- modelo$coefficients[2] %>%
    as.numeric()
}

media_dist_muestral_corr <- mean(muestras_corr)
EE_dist_muestral_corr <- sd(muestras_corr)

dist_muestral_corr <- ggplot(
  data = tibble(x = muestras_corr), 
  aes(x = x, y = stat(density*width))) +
  geom_histogram() + 
  labs(title="Distribución Muestral",
       x="Coeficiente", 
       y = "Proporción de encuestas")

dist_muestral_corr