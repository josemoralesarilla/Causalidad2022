rm(list=ls()) # removes all objects from the environment
cat('\014') # clears the console\

###Load packages
library(haven)
library(tree)
library(ranger)
library(rpart)
library(ggplot2)
library(fixest)
library(tidyverse)
library(caret)

########################################################
### Predecir la movilidad social en ciudades de EEUU ###
########################################################

## Define la raíz
set.seed(3)

## Cargar la data
mobility <- read_dta("mobility.dta")

## Eliminar variables sobrantes
mobility <- mobility %>% 
  select(-c('cz', 'state_id', 'stateabbrv', 'relative_mobility',
            'prob_p1_k5'))

# Imputar valores faltantes (si quieren sáltense esto)
preProcValues <- preProcess(as.data.frame(mobility %>% select(-czname)),
                            method = c("knnImpute"),
                            k = 10,
                            knnSummary = mean)
mobility <- predict(preProcValues, mobility,na.action = na.pass)

procNames <- data.frame(col = names(preProcValues$mean), mean = preProcValues$mean, sd = preProcValues$std)
for(i in procNames$col){
  mobility[i] <- mobility[i]*preProcValues$std[i]+preProcValues$mean[i] 
}

# Aleatorizar orden de las variables
order <- c(1, 2, sample(3:ncol(mobility)))
mobility <- mobility[, order] 

# Mobilidad social en una ciudad como función del número de boliches.
m1 <- feols(kfr_pooled_pooled_p25 ~ bowl_per_capita, data = mobility)
m1_r2 <- 1-m1$ssr/m1$ssr_null # R2 del modelo.
m1_rmse <- sqrt(m1$ssr/m1$nobs) # Raiz del promedio de los errores cuadrados.
mobility$y_hat <- m1$fitted.values # Agregar las predicciones a la data.
mobility$resid <- mobility$kfr_pooled_pooled_p25 - mobility$y_hat # Calcular residuos.
mobility$sq_resid <- (mobility$resid)^2 # Calcular residuos cuadrados.
m1_rmse_largo <- sqrt(sum(mobility$sq_resid)/dim(mobility)[1]) # Exactamente igual.

# ¿Qué le pasa al RMSE si agrego una variable más, tipo el número de boliches al cuadrado?
m2 <- feols(kfr_pooled_pooled_p25 ~ 
              poly(bowl_per_capita, 2, raw = TRUE), data = mobility)
m2_rmse <- sqrt(m2$ssr/m2$nobs)

# ¿Y qué le pasa si agregamos el cubo?
m3 <- feols(kfr_pooled_pooled_p25 ~ 
              poly(bowl_per_capita, 3, raw = TRUE), data = mobility)
m3_rmse <- sqrt(m3$ssr/m3$nobs)

# ¿Cómo va cambiando el RMSE si lo llevamos hasta 10?
p <- c()
rmse <- c()
for(i in 1:10){
  m <- feols(kfr_pooled_pooled_p25 ~ 
               poly(bowl_per_capita, i, raw = TRUE), data = mobility)
  p <- c(p, i)
  rmse <- c(rmse, sqrt(m$ssr/m$nobs))
}
RMSE <- tibble(p, rmse)
RMSE %>% ggplot(aes(x = p, y = rmse)) + geom_point()

# Dale que no viene carro! Hagamos ahora lo mismo, pero con dos variables!
p <- c()
rmse <- c()
for(i in 1:10){
  m <- feols(kfr_pooled_pooled_p25 ~ 
               poly(bowl_per_capita, i, raw = TRUE) +
               poly(cs_fam_wkidsinglemom, i, raw = TRUE), data = mobility)
  p <- c(p, i)
  rmse <- c(rmse, sqrt(m$ssr/m$nobs))
}
RMSE <- tibble(p, rmse)
RMSE %>% ggplot(aes(x = p, y = rmse)) + geom_point()

# Puedes hacer de todo mientras el número de variables no sea mayor al número de observaciones!




# ¿Verdad? ¿Estamos haciendo lo que dijimos que quería hacer la gente de Netflix?




# No realmente. 
# La gente de Neflix quiere fijar un modelo que sea bueno prediciendo sobre data que...
# ...no se usó para fijar el modelo. 
# En este caso, estamos fijando el modelo sobre una data y estamos evaluando la calidad...
# ...de las predicciones sobre la misma data con la que se fijó el modelo!




# ¿Cómo podemos replicar el ejercicio de Netflix con la data que tenemos?
#     1. Dividir la data en dos grupos.
#     2. Fijar el modelo sobre una parte de la data.
#     3. Evaluar la calidad de las predicciones sobre la otra parte. 

set.seed(5)
mobility <- mobility[,1:40] # Eliminar nuevas variables

# Quedémonos con 70% de la data para "entrenar" el modelo y 30% para evaluarlo.
mobility$train_flag <- rbinom(dim(mobility)[1], 1, .7)
train <- subset(mobility, train_flag == 1) %>% select(-train_flag)
test <- subset(mobility, train_flag == 0) %>% select(-train_flag)

# Fijemos el modelo más sencillo sobre la data de entrenamiento.
m1_train <- feols(kfr_pooled_pooled_p25 ~ bowl_per_capita, data = train)
# Calculemos el RMSE sobre la data de entrenamiento.
m1_train_rmse <- sqrt(m1$ssr/m1$nobs)
# ¿Este es el valor que nos interesa para evaluar la calidad del modelo?
# No, nos interesa el RMSE sobre la data de evaluación (Test).
# ¿Cómo calculamos las predicciones sobre una data distinta? Con la función "predict".
test$y_hat <- predict(m1_train, test)
test$sq_resid <- (test$kfr_pooled_pooled_p25 - test$y_hat)^2
m1_test_rmse <- sqrt(sum(test$sq_resid)/dim(test)[1])

# Evaluemos expansiones polinómicas del número de boliches en la data de evaluación.
p <- c()
rmse_train <- c()
rmse_test <- c()
for(i in 1:10){
  m <- feols(kfr_pooled_pooled_p25 ~ 
               poly(bowl_per_capita, i, raw = TRUE) + 
               poly(cs_fam_wkidsinglemom, i, raw = TRUE), data = train)
  p <- c(p, i)
  rmse_train <- c(rmse_train, sqrt(m$ssr/m$nobs))
  test$y_hat <- predict(m, test)
  test$sq_resid <- (test$kfr_pooled_pooled_p25 - test$y_hat)^2
  rmse_test <- c(rmse_test, sqrt(sum(test$sq_resid)/dim(test)[1]))
}
RMSE <- tibble(p, rmse_train, rmse_test)
RMSE %>% ggplot(aes(x = p, y = rmse_train)) + geom_point() +
  geom_point(aes(x = p, y = rmse_test), color = 'red')

# Wow, ¿Qué pasó aquí? Vamos a ver la tabla de resultados:
View(RMSE)

# En esta data: ¿Cuál parece ser el orden polinómico ideal?

# Ahora veamos simplemente el efecto de meterle más variables al modelo sin expansiones polinómicas.

numero_variables <- c()
rmse_train <- c()
rmse_test <- c()
for(i in 15:25){
  df <- train[,2:i]
  form <- ""
  m <- lm(kfr_pooled_pooled_p25 ~ ., data = df)
  numero_variables <- c(numero_variables, as.numeric(i-2))
  
  train$y_hat <- predict(m, train)
  train$sq_resid <- (train$kfr_pooled_pooled_p25 - train$y_hat)^2
  rmse_train <- c(rmse_train, sqrt(sum(train$sq_resid, na.rm = TRUE)/dim(train)[1]))
  
  test$y_hat <- predict(m, test)
  n_test <- test %>% mutate(pred = if_else(!is.na(y_hat), 1, 0)) %>% 
    summarize(sum(pred)) %>% as.numeric()
  test$sq_resid <- (test$kfr_pooled_pooled_p25 - test$y_hat)^2
  rmse_test <- c(rmse_test, sqrt(sum(test$sq_resid, na.rm = TRUE)/dim(test)[1]))
}
RMSE <- tibble(p, rmse_train, rmse_test)
RMSE %>% ggplot(aes(x = numero_variables, y = rmse_train)) + geom_point() +
  geom_point(aes(x = numero_variables, y = rmse_test), color = 'red')

#################################################
### Problema de sobreajuste en una simulación ###
#################################################

data <- tibble(X = rnorm(100, 3, 1),
               Y = 3 * X - .5 * X^2 + rnorm(100, 10, 1))

data %>% ggplot(aes(x = X, y = Y)) + geom_point()

data %>% ggplot(aes(x = X, y = Y)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ x)

data %>% ggplot(aes(x = X, y = Y)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 2))

data %>% ggplot(aes(x = X, y = Y)) + geom_point() +
  geom_smooth(method = "lm", formula = y ~ poly(x, degree = 10))

train <- data[1:50,]
test <- data[51:100,]

p <- c()
rmse_train <- c()
rmse_test <- c()
for(i in 1:9){
  m <- feols(Y ~ poly(X, i, raw = TRUE), data = train)
  p <- c(p, i)
  rmse_train <- c(rmse_train, sqrt(m$ssr/m$nobs))
  test$y_hat <- predict(m, test)
  test$sq_resid <- (test$Y - test$y_hat)^2
  rmse_test <- c(rmse_test, sqrt(sum(test$sq_resid)/dim(test)[1]))
}
RMSE <- tibble(p, rmse_train, rmse_test)
RMSE %>% ggplot(aes(x = p, y = rmse_train)) + geom_point() +
  geom_point(aes(x = p, y = rmse_test), color = 'red')

###################################################
### ML para predicción del precio de las casas. ###
###################################################

library(tidyverse)
library(AmesHousing)
library(rsample)
library(glmnet)
library(scales)
library(ggthemes)

# Data ----
ames <- make_ames()

# Dividir entre entrenamiento y evaluación ----
set.seed(02138)
split <- initial_split(ames, prop = 0.70)
ames_train <- training(split)
ames_test <- testing(split)

# Pre-procesamiento para ejecutar métodos de ML -----
X_train <- model.matrix(Sale_Price ~ ., data = ames_train)[, -1] # Matriz de predictores.
y_train <- ames_train$Sale_Price # Vector de resultado.

# LASSO -----
fit_lasso <- cv.glmnet(x = X_train, y = y_train, 
                       alpha = 1, nfolds = 10)
plot(fit_lasso)

# Ridge -----
fit_ridge <- cv.glmnet(x = X_train, y = y_train, 
                       alpha = 0, nfolds = 10)
plot(fit_ridge)

# MCO ------
fit_mco <- glmnet(x = X_train, y = y_train, lambda = 0)

# Predicciones -----
X_test <- model.matrix(Sale_Price ~ ., data = ames_test)[, -1]

# Visualizando predicciones
# 1. LASSO
ames_test %>%
  mutate(
    pred_lasso = as.vector(predict(fit_lasso,
                                   newx = X_test,
                                   s = "lambda.min"))
  ) %>%
  ggplot(aes(x = Sale_Price, y = pred_lasso)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  coord_equal() +
  theme_clean() +
  labs(x = "Truth",
       y = "Prediction")

# 2. Ridge
ames_test %>%
  mutate(
    pred_lasso = as.vector(predict(fit_ridge,
                                   newx = X_test,
                                   s = "lambda.min"))
  ) %>%
  ggplot(aes(x = Sale_Price, y = pred_lasso)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  coord_equal() +
  theme_clean() +
  labs(x = "Truth",
       y = "Prediction")

# 3. MCO
ames_test %>%
  mutate(
    pred_lasso = as.vector(predict(fit_mco,
                                   newx = X_test,
                                   s = "lambda.min"))
  ) %>%
  ggplot(aes(x = Sale_Price, y = pred_lasso)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = dollar) +
  scale_y_continuous(labels = dollar) +
  coord_equal() +
  theme_clean() +
  labs(x = "Truth",
       y = "Prediction")

# RMSE fuera de muestra
# LASSO
rmse_lasso <- ames_test %>%
  mutate(
    pred = as.vector(predict(fit_lasso,
                                   newx = X_test,
                                   s = "lambda.min")),
    sq_error = (Sale_Price - pred)^2) %>%
  summarize(sum(sq_error, na.rm = TRUE)) %>%
  as.numeric()
rmse_lasso <- sqrt(rmse_lasso / dim(ames_test)[1])

# Ridge
rmse_ridge <- ames_test %>%
  mutate(
    pred = as.vector(predict(fit_ridge,
                             newx = X_test,
                             s = "lambda.min")),
    sq_error = (Sale_Price - pred)^2) %>%
  summarize(sum(sq_error, na.rm = TRUE)) %>%
  as.numeric()
rmse_ridge <- sqrt(rmse_ridge / dim(ames_test)[1])

# MCO
rmse_mco <- ames_test %>%
  mutate(
    pred = as.vector(predict(fit_mco,
                             newx = X_test,
                             s = "lambda.min")),
    sq_error = (Sale_Price - pred)^2) %>%
  summarize(sum(sq_error, na.rm = TRUE)) %>%
  as.numeric()
rmse_mco <- sqrt(rmse_mco / dim(ames_test)[1])
