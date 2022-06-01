# Efecto verdadero de X sobre Y = 5.
df <- tibble(
  U = rnorm(5000),
  Z = rbinom(5000, 1, .5), # Z es binario
  X = 3 * U + 4 * Z + rnorm(5000),
  Y = 5 * X + 7 * U + rnorm(5000))

# Correlación simple se equivoca
OLS <- feols(Y ~ X, df)$coefficients[2]

# Intentemos IV con instrumento binario.
# Paso 1: Efecto de Z sobre X (Primera etapa)
FS <- feols(X ~ Z, df)
fs_coef <- FS$coefficients[2]
df$X_hat <- df$X - FS$residuals 

# Paso 2: Efecto de Z spbre Y (Segunda etapa)
RF <- feols(Y ~ Z, df)
rf_coef <- RF$coefficients[2]
df$Y_hat <- df$Y - RF$residuals 

# Paso 3.1: Fracción de coeficientes
IV <-  rf_coef / fs_coef

# Paso 3.2: Regresión de valores predichos
IV2 <- feols(Y_hat ~ X_hat, df)$coefficients[2]
