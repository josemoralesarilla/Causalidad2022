## ----setup, include=FALSE-----------------------------------------------------------------------------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

# Install packages (if necessary) and load required libraries
if (!require(tidyverse)) install.packages("tidyverse"); library(tidyverse) 
if (!require(scales)) install.packages("scales"); library(scales) # formato de los ejes en ggplot
if (!require(patchwork)) install.packages("patchwork"); library(patchwork) # juntar ggplots
if (!require(AmesHousing)) install.packages("AmesHousing"); library(AmesHousing) # datos immobilarios
if (!require(rsample)) install.packages("rsample"); library(rsample) # para tomar muestras de datos
if (!require(glmnet)) install.packages("glmnet"); library(glmnet) # para LASSO
if (!require(haven)) install.packages("haven"); library(haven) # Datos de Stata
if (!require(randomForest)) install.packages("randomForest"); library(randomForest) #Random forests
if (!require(tree)) install.packages("rpart"); library(tree) # Primera opcion para arboles de decision
if (!require(rpart)) install.packages("rpart"); library(rpart) # Segunda opcion para arboles de decision



## ----load_ames----------------------------------------------------------------------------------------------------------------------------------------------------
# Data 
ames <- make_ames()

#names(ames)

head(ames)


## ----split_ames---------------------------------------------------------------------------------------------------------------------------------------------------

# Dividir entre entrenamiento y evaluación ----

# Fijamos una semilla para que la aleatorizacion siempre nos devuelva los mismos resultados
set.seed(02138)

# usamos el paquete rsample para hacer una division 70% training -30% testing
split <- rsample::initial_split(ames, prop = 0.70)
ames_train <- rsample::training(split)
ames_test <- rsample::testing(split)


## ----over_fit_ames------------------------------------------------------------------------------------------------------------------------------------------------
# Hablemos de over-fitting ----

ames_train<-ames_train %>% 
  # Cremos tres sub muestras
  mutate(random_id=runif(nrow(ames_train))) %>% 
  mutate(muestra=case_when(random_id<0.33~1,
                           random_id>=0.33 &random_id <0.66~2,
                           random_id>=0.66 &random_id <1~3))

# Estimaremos dos modelos para cada sub-muestra y despues los testearemos en ames_test

i<-1 # esto no hace falta, es para explicarles mejor como funciona el for loop 

model_reg<-list()
model_reg_poly<-list()

for (i in 1:3){

print(paste0("We're fitting the models for sample number ",i))

## Sepparate data into training and test
test <- subset(ames_train, muestra !=i )
train <- subset(ames_train, muestra == i)


model_reg[[i]] <- lm(Sale_Price ~  First_Flr_SF, 
                     data=train) 
model_reg_poly[[i]] <-lm(Sale_Price ~  First_Flr_SF + 
                       I(First_Flr_SF^2)+
                       I(First_Flr_SF^3)+
                       I(First_Flr_SF^4)+
                       I(First_Flr_SF^5)+
                       I(First_Flr_SF^6),
                    data=train)

}



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
## Inserta las predicciones de cada modelo en la data de testing

### Predicciones lineales
ames_test$pred_1<-predict(model_reg[[1]],newdata=ames_test)
ames_test$pred_2<-predict(model_reg[[2]],newdata=ames_test)
ames_test$pred_3<-predict(model_reg[[3]],newdata=ames_test)

### Predicciones polinomicas
ames_test$pred_poly_1<-predict(model_reg_poly[[1]],newdata=ames_test)
ames_test$pred_poly_2<-predict(model_reg_poly[[2]],newdata=ames_test)
ames_test$pred_poly_3<-predict(model_reg_poly[[3]],newdata=ames_test)


p2<-ggplot(data=ames_test, aes(y=Sale_Price,
                      x=First_Flr_SF))+
  geom_point(color="gray")+
  geom_line(aes(y=pred_1, color="Sample 1"), size=1)+
  geom_line(aes(y=pred_2, color="Sample 2"), size=1)+
  geom_line(aes(y=pred_3, color="Sample 3"), size=1)+
  scale_y_continuous(labels = scales::dollar,
                     limits = c(min(ames$Sale_Price),max(ames$Sale_Price)))+
  theme_minimal() +
  labs(y="Sale price",
       x="First Floor square feet", 
       color=NULL)

p3<-ggplot(data=ames_test, aes(y=Sale_Price,
                      x=First_Flr_SF))+
  geom_point(color="gray")+
  geom_line(aes(y=pred_poly_1, color="Sample 1"), size=1)+
  geom_line(aes(y=pred_poly_2, color="Sample 2"), size=1)+
  geom_line(aes(y=pred_poly_3, color="Sample 3"), size=1)+
  scale_y_continuous(labels = scales::dollar,
                     limits = c(min(ames$Sale_Price),max(ames$Sale_Price)))+
  theme_minimal() +
  labs(y="Sale price",
       x="First Floor square feet", 
       color=NULL)

# unificar graficos. Esto solo se puede hacer despues de ejecutar library(patchwork)
p2+p3+
  plot_layout(guides = "collect")+
  plot_annotation(title="Relatioship Sale price and Square foot",
                  subtitle = "Discussig bias-variance trade-off",
                  caption = "Lines show each model's predictions")



## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ames_train %>%
  filter(muestra==2)%>%
  # creamos columnas con las predicciones de cada modelo (solo seleccionamos algunos)
  mutate(
    pred_simple = predict(model_reg[[2]], newdata=subset(ames_train,muestra==2)),
    pred_poly = predict(model_reg_poly[[2]], newdata=subset(ames_train,muestra==2))
  ) %>%
  # creamos la variable RMSE. Fijate que usamos summarise porque es un calculo agregado sobre todos los datos. Es sencillo, pero ojo con los parentesis. 
  summarise(mean_sales_price=mean(Sale_Price),
            rmse_simple=sqrt(mean((Sale_Price-pred_simple)^2)),
            rmse_poly=sqrt(mean((Sale_Price-pred_poly)^2)))




## -----------------------------------------------------------------------------------------------------------------------------------------------------------------

ames_test %>%
  mutate(
    pred_simple = predict(model_reg[[2]], newdata=ames_test),
    pred_poly = predict(model_reg_poly[[2]], newdata=ames_test)
  ) %>%
  # creamos la variable RMSE. Fijate que usamos summarise porque es un calculo agregado sobre todos los datos. Es sencillo, pero ojo con los parentesis. 
  summarise(mean_sales_price=mean(Sale_Price),
            rmse_simple=sqrt(mean((Sale_Price-pred_simple)^2)),
            rmse_poly=sqrt(mean((Sale_Price-pred_poly)^2)))


## ----fit_lasso----------------------------------------------------------------------------------------------------------------------------------------------------
# Splitting ----
## Segmentaremos la data en 70%-30%
## Esta vez usaremos las funciones del paquete rsample::
set.seed(02138)

split <- rsample::initial_split(ames, prop = 0.70)
ames_train <- rsample::training(split)
ames_test <- rsample::testing(split)

# Pre-processing -----
# Debemos normalizar las variables que queremos que esten en el modelo. En este caso, implica ponerlas en un formato matricial especial.
# Recuerda que los [] son para seleccionar componentes de distintos objetos en R.
# Genericamente matriz[1:3,1:2] un comando asi te devolvera las 3 perimeras filas y las 2 primeras columnas de una matriz
# glmnet::
X_train <- model.matrix(Sale_Price ~ ., data = ames_train)[, -1]
y_train <- ames_train$Sale_Price


# LASSO -----
fit_lasso <- glmnet::cv.glmnet(x = X_train, y = y_train, 
                       alpha = 1, nfolds = 10)



## ----review_lasso-------------------------------------------------------------------------------------------------------------------------------------------------
# LASSO OUTPUTS ---
# Paremos un segundo para ver que es esto ?cv.glmnet 

# Grafico
plot(fit_lasso) # Eje x muestra todos los lamba simulados, y es el MSE, arriba esta la cantidad de variables seleccionadas en cada modelo, la primera linea muestra el lamda de minimo MSE

# Componentes
# este es un listado de la info contenida en el objeto fit_lasso
summary(fit_lasso)


# Resultados
# asi podemos extraer un data.frame con los coeficientes resultantes de la mejor estimacion
chosen_variables<-enframe(glmnet::coef.glmnet(fit_lasso, s="lambda.min")[,1])
head(chosen_variables)
chosen_variables %>% 
  filter(value!=0)



## ----lasso_predictions--------------------------------------------------------------------------------------------------------------------------------------------
# Prediction -----
X_test <- model.matrix(Sale_Price ~ ., data = ames_test)[, -1]

predictions_lasso<-predict(fit_lasso, newx = X_test,  s = "lambda.min") # fijate que predict es una funcion que cambia sus argumentos en funcion del tipo objeto sobre el que se aplica


ames_test %>%
  mutate(
    pred_lasso = as.vector(predictions_lasso)
  ) %>%
  ggplot(aes(x = Sale_Price, y = pred_lasso)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm")+
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  coord_equal() +
  theme_minimal() +
  labs(title = paste0("Linear model using ", 
                      nrow(filter(chosen_variables,value!=0)), 
                      " regularized attributes"),
       x = "Truth",
       y = "Prediction")


## ----rigge_mco_glm.net--------------------------------------------------------------------------------------------------------------------------------------------
# Ridge -----
# simplemente modifica el valor de alpha a cero
fit_ridge <- cv.glmnet(x = X_train, y = y_train, 
                       alpha = 0, nfolds = 10)

# MCO ------
# simplemente modifica el valor de lamda a cero: No habra penalidad adicional en la minimizacion de errores al cuadrado
fit_mco <- glmnet(x = X_train, y = y_train, lambda = 0)


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ames_test %>%
  mutate(
    pred_ridge = as.vector(predict(fit_ridge, newx = X_test,  s = "lambda.min"))
  ) %>%
  ggplot(aes(x = Sale_Price, y = pred_ridge)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  coord_equal() +
  theme_minimal() +
  labs(title = paste0("Linear model using ", 
                      nrow(filter(
                        enframe(glmnet::coef.glmnet(fit_ridge, s="lambda.min")[,1]),value!=0)), 
                      " regularized attributes"),
       x = "Truth",
       y = "Prediction",
       caption = "Comes from a Ridge regression")


## -----------------------------------------------------------------------------------------------------------------------------------------------------------------
ames_test %>%
  mutate(
    pred_mco = as.vector(predict(fit_mco, newx = X_test,  s = "lambda.min"))
  ) %>%
  ggplot(aes(x = Sale_Price, y = pred_mco)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  scale_x_continuous(labels = scales::dollar) +
  scale_y_continuous(labels = scales::dollar) +
  coord_equal() +
  theme_minimal() +
  labs(title = paste0("Linear model using ", 
                      nrow(filter(
                        enframe(glmnet::coef.glmnet(fit_mco, s="lambda.min")[,1]),value!=0)), 
                      " regularized attributes"),
       x = "Truth",
       y = "Prediction",
       caption = "Comes from a MCO regression")



## ----rmse_comparisson---------------------------------------------------------------------------------------------------------------------------------------------
ames_test %>%
  # creamos columnas con las predicciones de cada modelo (solo seleccionamos algunos)
  mutate(
    pred_lasso = as.vector(predictions_lasso),
    pred_ridge = as.vector(predict(fit_ridge, newx = X_test,  s = "lambda.min")),
    pred_mco = as.vector(predict(fit_mco, newx = X_test,  s = "lambda.min")),
    pred_simple = predict(model_reg[[2]], newdata=ames_test),
    pred_poly = predict(model_reg_poly[[2]], newdata=ames_test)
  ) %>%
  # creamos la variable RMSE. Fijate que usamos summarise porque es un calculo agregado sobre todos los datos. Es sencillo, pero ojo con los parentesis. 
  summarise(mean_sales_price=mean(Sale_Price),
            rmse_lasso=sqrt(mean((Sale_Price-pred_lasso)^2)),
            rmse_ridge=sqrt(mean((Sale_Price-pred_ridge)^2)),
            rmse_mco=sqrt(mean((Sale_Price-pred_mco)^2)),
            rmse_simple=sqrt(mean((Sale_Price-pred_simple)^2)),
            rmse_poly=sqrt(mean((Sale_Price-pred_poly)^2)))
# 

