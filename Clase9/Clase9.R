library(haven)
library(tidyverse)
library(Synth)
library(SCtools)

# Carga la data
cig <- read_dta("ec50_smoking.dta")

#-------------------------------------------------------------------------------
# Procesamiento de datos
#-------------------------------------------------------------------------------

# Crea el log del PIB per capita.
# Crea el indicador para California.
cig <- cig %>% mutate(
  lnincome = log(personal_income), 
  ca = if_else(state == "CA", 1, 0)
)

# Quédate solo con CA y estados que no tuvieron políticas similares
prop99 <- subset(cig, sample == 1)

# Quédate solo con data hasta el año 2000
cig_narrow <- subset(prop99, year <= 2000)

#-------------------------------------------------------------------------------
# Análisis gráfico
#-------------------------------------------------------------------------------

# Compara a California con el promedio de los otros estados
ggplot(cig_narrow, 
       aes(x = year, y = pack_sales, 
           shape = factor(ca, labels = 
                      c("Rest of U.S.", "California")))) +
  geom_vline(xintercept = 1988) +
  stat_summary(fun.y = "mean", geom = "point") +
  stat_summary(fun.y = "mean", geom = "line")  +
  labs(x = "Year", y = "Cigarette Consumption", shape = "") +
  theme(legend.position = "bottom")
ggsave("plot1.png")


#-------------------------------------------------------------------------------
# Estimación del control sintético
#-------------------------------------------------------------------------------

# Ejercicio basado en los siguientes documentos
#https://www.rdocumentation.org/packages/Synth/versions/1.1-5/topics/synth
#https://www.rdocumentation.org/packages/Synth/versions/1.1-5/topics/dataprep
#https://rpubs.com/danilofreire/synth


# Crea una lista de los ID de los estados distintos a CA
list_state <- unique(prop99$state_fips)
list_state <- list_state[-3] # El tercero en la lista es CA, ID 6.

# Convierte data en formato de data frame como insumo del comando.
prop99 <- as.data.frame(prop99)

# Prepara insumo para el comando
dataprep.out <-
  dataprep( # Comando de Synth para preparar insumos.
    foo = prop99, # Data de origen.
    predictors = c("beer", "lnincome", "cost_per_pack"), # Predictores
    predictors.op = "mean", # Nos interesa el promedio ponderado
    dependent = "pack_sales", # Resultado de interés es la venta de cigarrillos
    unit.variable = "state_fips", # ID de los individuos
    time.variable = "year", # ID del tiempo
    special.predictors = list( # Quieres predecir variable resultado antes del tratamiento
      list("pack_sales", 1975, "mean"),
      list("pack_sales", 1980, "mean"),
      list("pack_sales", 1988, "mean")
    ),
    treatment.identifier = 6, # ID de California
    controls.identifier = list_state, # IDs de potenciales controles.
    time.predictors.prior = c(1970:1988), # Período pre-tratamiento.
    time.optimize.ssr = c(1970:1988), # Período para la optimización.
    unit.names.variable = "state", # Nombre de los individuos.
    time.plot = 1970:2018 # Período completo para la figura
  )

## Corre el comando synth para crear el control sintético
synth.out <- synth(dataprep.out)

## ¿Qué porcentaje se asigna a cada estado?
round(synth.out$solution.w, 2)

## Capturemos la diferencia entre Cali y Cali sintética
gaps <- dataprep.out$Y1plot - 
  (dataprep.out$Y0plot %*% synth.out$solution.w) 
  # Control sintético: Producto matricial entre ventas de cada estado por el vector de pesos.

gaps

## ¿Cómo se compara Cali con Cali Sintética y el promedio de EEUU?
synth.tables <- synth.tab(
  dataprep.res = dataprep.out,
  synth.res = synth.out)
print(synth.tables)

## Gráfico de Cali vs. Cali Sintética
path.plot(dataprep.res = dataprep.out,synth.res = synth.out)

## Gráfico de las diferencias
gaps.plot(dataprep.res = dataprep.out,synth.res = synth.out)


#-------------------------------------------------------------------------------
# Tests de permutación
#-------------------------------------------------------------------------------

## Basado en la librería SCtools
## https://cran.r-project.org/web/packages/SCtools/SCtools.pdf

## Para tests de permutación tenemos que replicar el ejercicio
## para todos los otros estados. Esto va a tomar bastante tiempo.
## Ya yo lo ejecuté. Ustedes déjenlo corriendo.
tdf <- generate.placebos(dataprep.out, synth.out, Sigf.ipop = 2)
    # Si tienes prisa, usa Sigf.ipop = 2. 
    # Si tienes tiempo y quieres precisión, usa el default Sigf.ipop = 5.

## Compara diferencias con versiones sintéticas de California vs. Placebos
p <- plot_placebos(tdf,discard.extreme=TRUE, 
                   mspe.limit=10, xlab='Year')
p

## Compara predictibilidad antes y después del tratamientro entre Cali y placebos
ratio <- mspe.test(tdf)
ratio$p.val
mspe.plot(tdf, discard.extreme = FALSE)






