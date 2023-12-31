library(tidyverse)

casas <- read_csv("casas.csv")
glimpse(casas)

# 1. Condición y calidad general vs precio x m2

# haz una tabla de conteos de los valores
# de calidad general de construcción y terminados (calidad_gral)

# tip: usa la función tally o summarise
casas %>% group_by(calidad_gral) %>% 
  summarise(n = n())
casas %>% group_by(condicion_venta) %>% 
  summarise(n = n())
# Haz una gráfica de caja y brazos del precio x m2 para cada 
# nivel de calidad gral ¿Qué interpretas?

# aquí tu codigo (tip: usa factor(variable) para convertir una variable
# numérica a factor)
ggplot(casas, 
       aes(x = calidad_gral, y = precio_m2, group = calidad_gral)) +
  geom_boxplot() 
ggplot(casas, 
       aes(x = factor(calidad_gral), y = precio_m2)) +
  geom_boxplot() 
# 2. Repite el anterior con número de coches que caben en el garage.
#¿Cuál es la relación? ¿Qué puedes malinterpretar de esta gráfica?

# num_coches
ggplot(casas , 
       aes(x = factor(num_coches), y = precio_m2)) +
  geom_boxplot() + geom_jitter(width = 0.2)
casas %>% group_by(num_coches) %>% summarise(n = n())

# aquí tu respuesta

# 3. Tablas cruzadas: azúcar y tipo de té

#Lee los datos de tomadores de té
tea <- read_csv("tea.csv")

## Haz una tabla cruzada de uso de azúcar (sugar) con tipo de té (Tea),
## mostrando el número de casos en cada cruce
# Pon el uso de azúcar en las columnas

tea %>% # aquí tu código
  
## ¿Cómo se relaciona el uso de azúcar con el té que toman las personas?
## Haz una tabla de porcentajes por renglón (para cada tipo de té) de tu tabla anterior

tea %>% # aquí tu código
  
## 4. Haz una tabla cruzada para la variable Tea y la presentación (how)
## donde las columnas son el tipo de Té

tea %>% # tu código
  
## Ahora calcula porcentajes por columna de la tabla anterior

tea %>% # tu código aquí 
  
# ¿Cómo son diferentes en cuanto a la presentación
# los tomadores de distintos tés (negro, earl gray, etc.)?



