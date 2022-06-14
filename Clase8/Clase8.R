###########################################################
# Ejercicio de simulación
###########################################################

# Creemos un panel
data_panel <- tibble(obs = c(1:6000), # 6000 observaciones
               ind = rep(c(1:100), times = 60), # 100 individuos.
               year = rep(c(1961:2020), each = 100), # seguidos por 60 años.
               T = if_else(ind > 50, 1, 0), # solo últimos 50 fueron tratados.
               Post = if_else(year > 2000, 1, 0), # Tratamiento fue en el 2000.
               Y = 2 * T + # Las unidades tratadas son más ricas.
                 3 * year + # Tendencia positiva en Y.
                 rnorm(6000) # Término de error normal.
               ) %>%
  # Efecto causal del tratamiento (5) solo opera después de 2000.
  mutate(Y = if_else(T == 1 & Post == 1, Y + 5, Y)) 

# ¿Podemos estimar el efecto en sección cruzada?
data_sc <- data_panel %>% filter(year == 2020) # Tomemos un solo año
summary(data_sc) # No hay variación en Post, solo en T. 
feols(Y ~ T, data_sc) # Diferencia promedio de Y por T != ATE.

# ¿Podemos estimar el efecto en serie de tiempo?
data_st <- data_panel %>% filter(ind == 57)
summary(data_st) # No hay variación en T, solo en Post.
feols(Y ~ Post, data_st) # Diferencia promedio de Y por Post != ATE.

# ¿Podemos estimar el efecto en panel?
summary(data_panel) # Variación en T y en Post
feols(Y ~ T, data_panel) # T no captura ATE.
feols(Y ~ Post, data_panel) # Post no captura ATE.
feols(Y ~ T:Post, data_panel) # Interacción no captura efecto causal.
feols(Y ~ T*Post, data_panel) # Interacción captura efecto causal...
                              # ...si controlamos por T y Post.
feols(Y ~ T:Post | ind + year, 
      data_panel) # Con TWFE, Interacción sola captura efecto causal.
# Pregunta: ¿Por qué no hace falta controlar por T y Post cuando usamos EF?

# Cambio en la diferencia de medias entre grupos tratado y no tratado.
data_medias <- data_panel %>% group_by(T, Post) %>% summarise(Y = mean(Y))

d_p0 <- data_medias$Y[3] - data_medias$Y[1] # Diff entre grupos pre tratamiento
d_p1 <- data_medias$Y[4] - data_medias$Y[2] # Diff entre grupos post tratamiento
did <- d_p1 - d_p0 # Cambio en la diferencias (diff in diff)

# Diferencia en el cambio de las medias para cada grupo de ta lo mismo.
d_t0 <- data_medias$Y[2] - data_medias$Y[1] # Cambio en la media en grupo de control
d_t1 <- data_medias$Y[4] - data_medias$Y[3] # Cambio en la media en grupo de tratamiento
did_2 <- d_t1 - d_t0 # Diferencia en el cambio (diff in diff)


###########################################################
# Análisis de datos de Card y Krueger (1994)
###########################################################

# Data formato largo
mw <- as.tibble(read.table("mw_data/public.dat")) %>% 
  mutate(
    firm_id = V1,
    NJ = if_else(V8 == 0 & V9 == 0, 1, 0),
    E0 = as.numeric(V12) + as.numeric(V13),
    E1 = as.numeric(V32) + as.numeric(V33)) %>% 
  select(c(firm_id, NJ, E0, E1)) %>%
  pivot_longer(cols = E0:E1,
               names_to = "Post",
               names_prefix = "E",
               values_to = "E")

# Regresión
reg <- feols(E ~ NJ * Post, mw)
reg$coefficients[1] # Tamaño restaurant promedio en Penn previo tratamiento
reg$coefficients[2] # Diferencia promedio de tamaño entre NJ y Penn antes del tratamiento
reg$coefficients[3] # Crecimiento de empleo promedio entre empresas de Penn
reg$coefficients[4] # Cambio en la diferencia entre NJ y Penn entre pre y post

mw_medias <- mw %>% group_by(NJ, Post) %>% summarize (E = mean(E))
mw_medias$E[1]
mw_medias$E[3]-mw_medias$E[1]
mw_medias$E[2]-mw_medias$E[1]
(mw_medias$E[4]-mw_medias$E[2])-(mw_medias$E[3]-mw_medias$E[1])

# Data formato ancho
mw2 <- as.tibble(read.table("mw_data/public.dat")) %>% 
  mutate(
    firm_id = V1,
    NJ = if_else(V8 == 0 & V9 == 0, 1, 0),
    E0 = as.numeric(V12) + as.numeric(V13),
    E1 = as.numeric(V32) + as.numeric(V33),
    DE = E1/E0) %>% 
  select(c(firm_id, NJ, DE, E0, E1))

feols(DE ~ NJ, mw2)

###########################################################
# Earned Income Tax Credit
###########################################################

# Importa la data, identifica unidades tratadas y periodo post
df <- read_csv('http://nickchk.com/eitc.csv') %>%
  mutate(T = children > 0,
         Post = year >= 1994)

# ¿Como cambian las medias antes y despues del tratamiento?
plotdata <- df %>% group_by(T, year) %>%
  summarize(work = mean(work))

ggplot(plotdata, aes(x = year, y = work, color = T)) + 
  geom_line() + 
  geom_vline(aes(xintercept = 1994))

# ¿Como cambian las diferencias de medias entre tratados y control?
plotdata_wide <- plotdata %>% 
  pivot_wider(names_from = "T",
              names_prefix = "T",
              values_from = "work") %>%
  mutate(diff = TTRUE - TFALSE)

ggplot(plotdata_wide, aes(x = year, y = diff)) + 
  geom_line() + 
  geom_vline(aes(xintercept = 1994))

# ¿Cuál es el efecto del tratamiento vía DiD?
feols(work ~ T*Post | state + year, df)

# Testeemos tendencias paralelas:
df_pre <- df %>% filter(year <= 1994)
feols(work ~ year * T, df_pre)

# Placebo post:
df_pre <- df_pre %>% mutate(Post = year > 1993)
feols(work ~ T*Post | state + year, df_pre)

# DiD Dinámico:
df <- df %>% mutate(yr = relevel(factor(year), ref = 3),
                    T = if_else(children > 0, 1, 0))
m <- feols(work ~ T * yr | state + yr + T, df )

coefplot(m, ref = c('1993' = 3), pt.join = TRUE, ci_level = .95)
