## ---- librerias------------------------------------------------------------------
library(tidyverse)
library(haven)
library(Synth)
library(devtools)
library(SCtools)

texas <- read_dta("https://raw.github.com/scunning1975/mixtape/master/texas.dta") %>%
  as.data.frame(.)


## ----raw_viz---------------------------------------------------------------------

head(texas)

texas %>% 
  # agrupemos los datos segun: Texas - todo lo demas
  # creamos la variable Texas- todo lo demas con
  mutate(categoria=ifelse(state=="Texas","Texas","USA (sin TX)")) %>% 
  # usamos group by por ano y categoria
  group_by(year,categoria) %>% 
  # calculamos la poblacion total y la cantidad de prisioneros total por cruce ano-categoria
  summarise(bmprison=sum(bmprison),
            bmpop=sum(bmpop)) %>% 
  # graficamos la tasa de encarcelacion por cada 100.000 habitantes
  ggplot(aes(x=year, y=bmprison/bmpop*100000, group=categoria))+
  geom_line(aes(color=categoria, linetype=categoria))+
  geom_vline(xintercept = 1993)+
  theme_minimal()+
  labs(title = "Black men incarceration rates per 100.000",
       subtitle = "Texas vs US",
       y="Black men incarceration rates")


## ----syntetic_controls-----------------------------------------------------------

# Prepara el objeto que servira como base para el analisis de controles sinteticos
dataprep_out <- Synth::dataprep(
  foo = texas,                          # data
  predictors = c("poverty", "income"),  # predictores
  predictors.op = "mean",               # promedio 
  time.predictors.prior = 1985:1993,    # Período para estimacion
  special.predictors = list(            # segmentacion de la prediccion por variable
    list("bmprison", c(1988, 1990:1992), "mean"), # fijate que controlamos por pre-treatment outcomes (more or less)
    list("alcohol", 1990, "mean"),
    list("aidscapita", 1990:1991, "mean"),
    list("black", 1990:1992, "mean"),
    list("perc1519", 1990, "mean")),
  dependent = "bmprison",               # variable dependiente
  unit.variable = "statefip",           # unidad en el panel de datos
  unit.names.variable = "state",         
  time.variable = "year",               # variable de tiempo en el panel de datos
  treatment.identifier = 48,            # categoria de refeerencia
  controls.identifier = c(1,2,4:6,8:13,15:42,44:47,49:51,53:56), # ids de los estados control. 
  time.optimize.ssr = 1985:1993, # Período para la optimización.
  time.plot = 1985:2000          # Período completo para la figura
)

## Corre el comando synth para crear el control sintético
synth_out <- Synth::synth(dataprep_out)

## Veamos los pesos mas altos (top 5)

# extraigo los pesos del objeto synth_out
pesos<-round(synth_out$solution.w*100,2)
fip<-as.numeric(dimnames(synth_out$solution.w)[[1]]) # los pesos tienen un atributo, en el panel de objetos a la derecha podemos darnos cuenta de eso

# creamos una pequena tabla juntando dos vectores en un data.frame
data.frame(statefip=fip, pesos) %>% 
  top_n(5,w.weight) %>% 
  left_join(distinct(texas,state,statefip))


## Capturemos la diferencia entre Texas y Texas sintética
gaps <- dataprep_out$Y1plot -   (dataprep_out$Y0plot %*% synth_out$solution.w) 
# Y1plot: tasa de encarcelamiento de Texas
# Control sintético: Producto matricial entre tasa de encarcelamiento de cada estado y el vector de pesos.
# Resulta en un Promedio ponderado
gaps



## ----levels_chart----------------------------------------------------------------
# Comparacion en niveles: Texas vs Texas sintetica
path.plot(synth_out, dataprep_out)



## ----gap_chart-------------------------------------------------------------------
# Visualizacion de brecha
gaps.plot(synth_out, dataprep_out)


## ----placebos, echo=FALSE--------------------------------------------------------
placebos <- generate.placebos(dataprep_out, synth_out, Sigf.ipop = 2)
# Si tienes prisa, usa Sigf.ipop = 2. 
# Si tienes tiempo y quieres precisión, usa el default Sigf.ipop = 3.



## --------------------------------------------------------------------------------
## Compara predictibilidad antes y después del tratamientro entre Texas y placebos
ratio <- mspe.test(placebos) 

ratio$test %>% 
  arrange(desc(MSPE.ratios)) # ranking the MSPE post/MSPE pre

ratio$p.val # p valor



## --------------------------------------------------------------------------------
plot_placebos(placebos)

mspe.plot(placebos, discard.extreme = TRUE, mspe.limit = 1, plot.hist = TRUE)

