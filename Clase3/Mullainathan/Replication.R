# Carga las librerias
library(tidyverse)
library(haven)
library(fixest)

# 1. Carga la data de Bertrand y Mullainathan (2004)
df <- read_dta("Mullainathan/lakisha_aer.dta")
df <- df %>% mutate(black = if_else(race == "b", 1, 0),
                    white = if_else(race == "w", 1, 0),
                    c_black = if_else(race == "b" & call == 1, 1, 0),
                    c_white = if_else(race == "w" & call == 1, 1, 0))

# 2. Calcula el total de CVs y de llamadas por grupo racial
t <- df %>% summarize(black = sum(black),
                           white = sum(white),
                           c_black = sum(c_black),
                           c_white = sum(c_white))

# 3. Calcula la proporcion de llamadas por grupo racial
prop_c_black <- t$c_black / t$black
prop_c_white <- t$c_white / t$white

# 4. Calcula el ratio y la diferencia en la proporcion de llamadas
ratio <- prop_c_white / prop_c_black
diff <- prop_c_black - prop_c_white

# 5. Evalua si la diferencia en las proporciones es significativa
prop.test(x = c(t$c_black[1], t$c_white[1]), 
          n = c(t$black[1], t$white[1]))

# 6. Corre una regresion de la probabilidad de llamada por raza
feols(call ~ black, df)

# 7. Evalua si la aleatorizacion de la raza funciono
vars <- c("call", "honors", "email",
          "military", "volunteer", "empholes")
COEFS <- c()
CI_lo <- c()
CI_hi <- c()
for (i in 1:length(vars)){
  Y <- vars[i]
  mod <- feols(as.formula(paste(vars[i],'~ black')), df)
  coef <- mod$coefficients[2]
  se <- mod$se[2]
  ci_lo <- coef - 1.96 * se
  ci_hi <- coef + 1.96 * se
  COEFS <- c(COEFS,coef)
  CI_lo <- c(CI_lo, ci_lo)
  CI_hi <- c(CI_hi, ci_hi)
}

baseline_tests <- tibble(
  variables = vars, beta = COEFS,
  ci_lo = CI_lo, ci_hi = CI_hi,
  null = if_else(ci_lo < 0 & ci_hi > 0, 1, 0)
)

ggplot(baseline_tests, aes(y = variables, x = beta)) +
  geom_point() + geom_errorbarh(
    aes(xmin = ci_lo, xmax = ci_hi), 
    height = 0) +
  geom_vline(xintercept = 0, linetype = 'dashed')

# 8. Evalua los efectos de la calidad del CV por grupo racial
df_black <- df %>% filter(black == 1)
df_white <- df %>% filter(black == 0)

feols(call ~ h, df_white)
feols(call ~ h, df_black)


