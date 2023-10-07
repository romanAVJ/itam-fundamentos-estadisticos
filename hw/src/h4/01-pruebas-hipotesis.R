library(tidyverse)
library(nullabor)

propinas <- read_csv("propinas.csv")

# Prueba de sospechosos (prueba de hipótesis visual)
# ¿Las propinas son diferentes entre cena y comida (momento)

# 1. Crea permutaciones
# ¿Cómo se ve la tabla perms_momento?
perms_momento <- lineup(null_permute("momento"), propinas, n = 12)

# Haz una grafica de caja y brazo de momento contra propinas
# separando en páneles las permutaciones

ggplot(perms_momento, aes(x=#llenar, y = #llenar)) +
  geom_boxplot() + facet_wrap(#llenar) 

# puedes identificar los datos verdaderos?
# usa el comando decrypt que salió arriba para averiguar
# dónde están los datos

decrypt(" aquí tu código")


# qué tanta evidencia crees que aporta este análisis
# en contra de la hipótesis que las propinas son similares
# en niveles en comida y cena)

#------------------

# 2. Relaciones lineales
# ¿Está relacionado el tamaño de la cuenta con el tamaño de la propina?

perms_propina <- lineup(null_permute("propina"), propinas, n = 12)

# Haz una grafica de puntos de tamaño de cuenta vs propina
# separando en páneles las permutaciones

ggplot(perms_propina, aes(x=#rellenar, y = #rellenar)) +
  geom_point(size = 0.5) + facet_wrap(#rellenar) 

# cuánta evidencia tienes en contra de que no están relacionados?

#--------------------
#3. Haz una prueba de diferencia de medias
# para comparar la propina en cena vs en comidas

momento_propina_tbl <- propinas %>% select(momento, propina)
# cinco mil permutaciones
perms_propina <- lineup(null_permute("propina"), 
                        momento_propina_tbl, n = 5000)

# resumimos y calculamos diferencia
# revisa lo que obtienes en cada paso del siguiente código
resumen <- perms_propina %>% 
  group_by(momento, .sample) %>% 
  summarise(media = mean(propina)) %>% 
  pivot_wider(names_from = momento, values_from = media) %>% 
  mutate(dif_cena_comida = Cena - Comida)

# ahora grafica distribución (histograma, geom_histogram) 
# de simulaciones contra el 
# valor en los datos
ggplot(#rellena)



# Calcula la diferencia en los datos es
momento_propina_tbl %>% ## calcula
dif_obs <- # rellenar el número

ggplot(resumen, aes(x = dif_cena_comida)) + geom_histogram() +
  geom_vline(xintercept = dif_obs, colour = "red")

# y el valor p (dos colas)
dist_ref <- ecdf(resumen$dif_cena_comida)

valor_p <- 2 * min(dist_ref(dif_obs), (1 - dist_ref(dif_obs)))
valor_p

## ¿cuál es tu conclusión?