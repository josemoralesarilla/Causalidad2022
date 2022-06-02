library(tidyverse)  # ggplot(), %>%, mutate(), and friends
library(broom)  # Convert models to data frames
library(rdrobust)  # For robust nonparametric regression discontinuity
library(estimatr)  # Run 2SLS models in one step with iv_robust()
library(modelsummary)  # Create side-by-side regression tables
library(kableExtra)
library(fixest)

tutoring <- read_csv("tutoring.csv")

# ¿Cuál es la diferencia en notas finales entre los tutoreados y no tutoreados?
feols(exit_exam ~ tutoring, tutoring)

# Visualizando imperfección en aplicación de la regla (1)
ggplot(tutoring, aes(x = entrance_exam, y = tutoring_text, color = entrance_exam <= 70)) +
  # Make points small and semi-transparent since there are lots of them
  geom_point(size = 1.5, alpha = 0.5, 
             position = position_jitter(width = 0, height = 0.25, seed = 1234)) + 
  # Add vertical line
  geom_vline(xintercept = 70) + 
  # Add labels
  labs(x = "Nota en examen de entrada", y = "Participación en programa de tutoría") + 
  # Turn off the color legend, since it's redundant
  guides(color = FALSE)

# Visualizando imperfección en aplicación de la regla (2)
tutoring_with_bins <- tutoring %>% 
  mutate(exam_binned = cut(entrance_exam, breaks = seq(0, 100, 5))) %>% 
  # Group by each of the new bins and tutoring status
  group_by(exam_binned, tutoring) %>% 
  # Count how many people are in each test bin + used/didn't use tutoring
  summarize(n = n()) %>% 
  # Make this summarized data wider so that there's a column for tutoring and no tutoring
  pivot_wider(names_from = "tutoring", values_from = "n", values_fill = 0) %>% 
  rename(tutor_yes = `TRUE`, tutor_no = `FALSE`) %>% 
  # Find the probability of tutoring in each bin by taking 
  # the count of yes / count of yes + count of no
  mutate(prob_tutoring = tutor_yes / (tutor_yes + tutor_no))

ggplot(tutoring_with_bins, aes(x = exam_binned, y = prob_tutoring)) +
  geom_col() +
  geom_vline(xintercept = 8.5) +
  labs(x = "Nota en examen de entrada", y = "Proporción de participantes en tutoría")

# Construye una variable de asignación centrada 
# y del grupo asignado a tratamiento bajo la regla:
tutoring <- tutoring %>% 
  mutate(entrance_centered = entrance_exam - 70,
         below_cutoff = entrance_exam <= 70)

# Construye una variable de la interacción entre 
# asignación al tratamiento bajo la regla y la variable de asignación.
tutoring <- tutoring %>% 
  mutate(interaction = 
           entrance_centered * below_cutoff)

# Construye una banda de 10 puntos alrededor de la discontinuidad.
tutoring <- tutoring %>% 
  mutate(en_banda = if_else(
    abs(entrance_centered) <= 10, 1, 0))

# Estima el efecto de la discontinuidad sobre el examen final.
# Banda = 10 puntos, 0 controles. 
rf0 <- feols(exit_exam ~ below_cutoff, 
      tutoring %>% filter(en_banda == 1))

# Banda = 10 puntos, Controles lineales. 
rf1 <- feols(exit_exam ~ below_cutoff + entrance_centered + interaction, 
      tutoring %>% filter(en_banda == 1))

# Estima el efecto de la discontinuidad sobre la probabilidad de tutoría.
# Banda = 10 puntos, 0 controles. 
fs0 <- feols(tutoring ~ below_cutoff, 
      tutoring %>% filter(en_banda == 1))

# Banda = 10 puntos, Controles lineales. 
fs1 <- feols(tutoring ~ below_cutoff + entrance_centered + interaction, 
      tutoring %>% filter(en_banda == 1))

# IV del efecto estimado de la tutoría para los "compliers".
# Banda = 10 puntos, 0 controles. 
iv0_frac <- rf0$coefficients[2] / fs0$coefficients[2]
iv0_mod <- feols(exit_exam ~ 1 | tutoring ~ below_cutoff,
                 tutoring %>% filter(en_banda == 1))
iv0_frac
iv0_mod$coefficients[2]

# Banda = 10 puntos, Controles lineales. 
iv1_frac <- rf1$coefficients[2] / fs1$coefficients[2]
iv1_mod <- feols(exit_exam ~ entrance_centered + interaction | tutoring ~ below_cutoff,
                 tutoring %>% filter(en_banda == 1))
iv1_frac
iv1_mod$coefficients[2]
