library(tidyverse)
library(patchwork)
## Lee los datos
tips <- read_csv("tips.csv")
glimpse(tips)

## Recodificar nombres y niveles
propinas <- tips %>% 
  rename(cuenta_total = total_bill, 
         propina = tip, sexo = sex, 
         fumador = smoker,
         dia = day, momento = time, 
         num_personas = size) %>% 
  mutate(sexo = recode(sexo, Female = "Mujer", Male = "Hombre"), 
         fumador = recode(fumador, No = "No", Si = "Si"),
         dia = recode(dia, Sun = "Dom", Sat = "Sab", Thur = "Jue", Fri = "Vie"),
         momento = recode(momento, Dinner = "Cena", Lunch = "Comida")) %>% 
  select(-sexo) %>% 
  mutate(dia  = fct_relevel(dia, c("Jue", "Vie", "Sab", "Dom")))
propinas


## 1. Calcula percentiles de la variable propina
## junto con mínimo y máxmo
quantile(propinas$propina, probs = seq(0, 1, 0.05))
  
## 2. Haz una gráfica de cuantiles de la variable propina
propinas <- propinas %>% 
  mutate(orden_propina = rank(cuenta_total, ties.method = "first"), 
         f = orden_propina / n()) 
## aquí tu código
# ggplot

ggplot(propinas, aes(x = cuenta_total)) +
  geom_histogram()

## 3. Haz un histograma de la variable propinas
## Ajusta distintos anchos de banda
# ggplot


## 4. Haz una gráfica de cuenta total contra propina
## ggplot


## 5. Calcula propina en porcentaje de la cuenta total
## calcula algunos cuantiles de propina en porcentaje
propinas <- propinas %>% 
  mutate(pct_propina = ### )
           
quantile(###)
  
  
## 6. Haz un histograma de la propina en porcentaje. Prueba con
##  distintos anchos de banda. 
ggplot()

## 7. Describe la distribución de propina en pct. ¿Hay datos atípicos?


##8. Filtra los casos con porcentaje de propina muy altos. 
## ¿Qué tipos de cuentas son? ¿Son cuentas grandes o chicas?
filter()

## 9. Haz una diagrama de caja y brazos para 
## propina en dolares dependiendo del momento (comida o cena)
## ¿Cuál parece más grande? ¿Por qué? Haz otras gráficas si es necesario.
ggplot()


