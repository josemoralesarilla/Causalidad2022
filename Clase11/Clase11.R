rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console\

###Load packages
if (!require(haven)) install.packages("haven"); library(haven)
if (!require(rpart)) install.packages("rpart"); library(rpart)
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse)
if (!require(randomForest)) install.packages("randomForest"); library(randomForest)

# Directorio
setwd("~/Dropbox/PhD/Teaching/Causal Inference 2021/Documents/Clase11")

###################################
####### Arboles de decisión #######
###################################

# Leer datos
mobility <- read_dta("mobility.dta")

# Separar en entrenamiento y evaluación
set.seed(123)
mobility$train_flag <- runif(741) < 0.7
test <- subset(mobility, train_flag == F)
train <- subset(mobility, train_flag == T)

# Arbol de profundidad 1
mobilitytree <- rpart(kfr_pooled_pooled_p25 ~ 
                        bowl_per_capita + cs_fam_wkidsinglemom, 
                      data=subset(mobility, train_flag==T), 
                      maxdepth = 1, cp=0) 

  # Visualizar características del arbol
plot(mobilitytree)
text(mobilitytree)

  # Error de predicción en data de evaluación
test$hat1 <- predict(mobilitytree, newdata=test)
squarederror <- (test$kfr_pooled_pooled_p25 - test$hat1)^2
sqrt(mean(squarederror, na.rm=TRUE))

# Arbol de profundidad 2
mobilitytree <- rpart(kfr_pooled_pooled_p25 ~ 
                        bowl_per_capita + cs_fam_wkidsinglemom, 
                      data=subset(mobility, train_flag==T), 
                      maxdepth = 2, cp=0) 
  # Visualizar resultado
plot(mobilitytree)
text(mobilitytree)


# Otros parámetros que se pueden definir
help("rpart.control")


# ¿Qué pasa con el error dentro de la muestra cuando sube la profundidad?
# ¿Qué pasa con el error fuera de la muestra cuando sube la profundidad?
# Evaluemos con profundidades de 1 a 20
test_loop<-rep(0, 20)
train_loop<-rep(0, 20)

# Loop para calcular error en muestra y fuera de muestra para cada profundidad.
for (i in 1:20){
  # Fija el modelo
  reg_i<-rpart(kfr_pooled_pooled_p25 ~ 
                 bowl_per_capita + cs_fam_wkidsinglemom, 
               data=subset(mobility, train_flag==T), 
               maxdepth = i, cp=0,  minsplit = 1, minbucket = 1) 
  
  # Error en muestra
  hat1 <- predict(reg_i, newdata=train)
  se1 <- (train$kfr_pooled_pooled_p25 - hat1)^2
  train_loop[i]<-sqrt(mean(se1, na.rm=TRUE))
  
  # Error fuera de muestra
  hat2 <- predict(reg_i, newdata=test)
  se2 <- (test$kfr_pooled_pooled_p25 - hat2)^2
  test_loop[i]<-sqrt(mean(se2, na.rm=TRUE))
}


# Crea data para visualización
depth <- c(1:20)
dataforgraph <- data.frame(test_loop, train_loop, depth)
names(dataforgraph)<-c("rmse.test","rmse.train","depth")

# Visualiza errores en muestra y fuera de muestra
ggplot(data=dataforgraph) +
  geom_point(aes(x=depth, y=rmse.train), colour = "blue") + 
  geom_line(aes(x=depth, y=rmse.train), colour = "blue") +
  geom_point(aes(x=depth, y=rmse.test), colour = "red") +
  geom_line(aes(x=depth, y=rmse.test), colour = "red")  +
  theme(legend.position = "none") +
  labs(title = "Prediciendo mobilidad social con arbol de decisión",
       y = "Root Mean Squared Error",
       x = "Profundidad del arbol",
       colour = "")

# Queremos capturar profundidad óptima en data de entrenamiento.
# Hagámoslo a través de la validación cruzada!
mobilitytree <- rpart(kfr_pooled_pooled_p25 ~ 
                        bowl_per_capita + cs_fam_wkidsinglemom,
                      data=train, control = rpart.control(xval = 10))
plot(mobilitytree) # plot tree
text(mobilitytree) # add labels to tree

# Error en muestra del modelo óptimo
train$rank_hat_tree <- predict(mobilitytree, newdata=train)
sqrt(mean((train$kfr_pooled_pooled_p25 - train$rank_hat_tree)^2, na.rm = TRUE))

# Error fuera de muestra del modelo óptimo
test$rank_hat_tree <- predict(mobilitytree, newdata=test)
sqrt(mean((test$kfr_pooled_pooled_p25 - test$rank_hat_tree)^2, na.rm = TRUE))

# El chiste de ML es poder usar todas las variables de entrada.
# Fijemos el arbol con todas las variables.

  # Primero quitemos las variables de identificación que no necesitamos.
train2 <- train %>% select(-c(cz, czname, state_id, stateabbrv,
                              relative_mobility, prob_p1_k5, train_flag,
                              rank_hat_tree))

  # Fijemos el modelo con todas las variables y con validación cruzada.
mobilitytree <- rpart(kfr_pooled_pooled_p25 ~ .,
                      data=train2, control = rpart.control(xval = 10))
  # Visualicemos el resultado
plot(mobilitytree) 
text(mobilitytree) 

# Evaluemos el error del modelo resultante fuera de muestra
test$rank_hat_tree <- predict(mobilitytree, newdata=test)
sqrt(mean((test$kfr_pooled_pooled_p25 - test$rank_hat_tree)^2, na.rm = TRUE))

###########################
###### RANDOM FOREST ######
###########################

# Abre una nueva data
atlas_training <- read_dta("atlas_training.dta")

# Guarda variables predictoras (empiezan con "P_")
vars <- colnames(atlas_training[,grep("^[P_]", names(atlas_training))])

# Quédate solo con la data de entrenamiento. 
training <- subset(atlas_training, training==1, vars)
training$kfr_pooled_pooled_p25 <- 
  atlas_training[atlas_training$training==1,]$kfr_pooled_pooled_p25

# Modelo MCO con todas las variables
mobilityreg <- lm(kfr_pooled_pooled_p25 ~ ., data=training)

# Arbol de decisión con validación cruzada
mobilitytree <- rpart(kfr_pooled_pooled_p25 ~ .
                      , data=training
                      , control = rpart.control(xval = 10))

# Bosque aleatorio con 500 muestras con reposición.
# Límite default: Mínimo 5 observaciones por grupo.
mobilityforest <- randomForest(kfr_pooled_pooled_p25 ~ ., 
                               ntree=500, 
                               importance=TRUE, 
                               data=training)

# Representación del bosque
getTree(mobilityforest, 250, labelVar = TRUE) 


# Lee data de evaluación
atlas_test <- read_dta("atlas_test.dta")

# Júntala con data de entrenamiento
atlas <- left_join(atlas_test, atlas_training , by="geoid")

# Separa la data de evaluación con variable resultado.
test <- subset(atlas, training==0)

# Obten predicciones sobre data de evaluación de cada modelo
rank_hat_forest <- predict(mobilityforest, newdata=test, type="response")
rank_hat_tree <- predict(mobilitytree, newdata=test)
hat_ols <- predict(mobilityreg, newdata=test)

# Calcula el RMSE fuera de muestra para cada modelo.
p <- 3
RMSE <- matrix(0, p, 1)
RMSE[1] <- sqrt(var(test$kfr_actual - hat_ols, na.rm=TRUE))
RMSE[2] <- sqrt(var(test$kfr_actual - rank_hat_tree, na.rm=TRUE))
RMSE[3] <- sqrt(var(test$kfr_actual - rank_hat_forest, na.rm=TRUE))

data_for_graph <- data.frame(RMSE, c("OLS", "CART", "RF"))  

# Cambia nombre de primera variable a "RMSE"
names(data_for_graph)[1] <- "RMSE"

# Cambia nombre de segunda variable a "Method"
names(data_for_graph)[2] <- "Method"

# Gráfico de barra simple
p <- ggplot(data=data_for_graph, aes(x=Method, y=RMSE)) +
  geom_bar(stat="identity")
# Imprímelo de forma horizontal
p + coord_flip()







