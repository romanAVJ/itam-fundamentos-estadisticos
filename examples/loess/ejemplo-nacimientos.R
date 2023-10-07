# Código de ejemplo en clase

## Lectura y limpieza
library(tidyverse)
library(lubridate)
library(patchwork) # paquete para acomodora dos o más gráficas
theme_set(theme_minimal(base_size = 14)) # tema para ggplot fondo blanco

natalidad <- read_rds("natalidad.rds") |>
  mutate(dia_semana = weekdays(fecha)) |>
  mutate(dia_año = yday(fecha)) |>
  mutate(año = year(fecha)) |>
  mutate(mes = month(fecha)) |>
  ungroup() |>
  mutate(dia_semana = recode(dia_semana, Monday = "Lunes", Tuesday = "Martes", Wednesday = "Miércoles",
                             Thursday = "Jueves", Friday = "Viernes", Saturday = "Sábado", Sunday = "Domingo")) |>
  #necesario pues el LOCALE puede cambiar
  mutate(dia_semana = recode(dia_semana, lunes = "Lunes", martes = "Martes", miércoles = "Miércoles",
                             jueves = "Jueves", viernes = "Viernes", sábado = "Sábado", domingo = "Domingo")) |>
  mutate(dia_semana = fct_relevel(dia_semana, c("Lunes", "Martes", "Miércoles",
                                                "Jueves", "Viernes", "Sábado", "Domingo")))


# gráfica de los datos crudos
ggplot(natalidad, aes(x = fecha, y = n)) +
  geom_line(alpha = 0.2) +
  geom_point(alpha = 0.5) +
  ylab("Nacimientos")

### tendencia
mod_1 <- loess(n ~ as.numeric(fecha), data = natalidad, span = 0.2, degree = 1)
datos_dia <- natalidad |> 
  mutate(ajuste_1 = fitted(mod_1)) |>
  mutate(res_1 = n - ajuste_1)

# probando distintos niveles de suavizamiento
g_1 <- ggplot(datos_dia, aes(x = fecha)) +
  geom_point(aes(y = n), alpha = 0.2, size = 1) +
  geom_line(aes(y = ajuste_1), colour = "red", linewidth = 1.2) + xlab("") +
  labs(caption = "Suavizamiento apropiado")
g_2 <- ggplot(datos_dia, aes(x = fecha, y = n)) +
  geom_point(alpha = 0.2, size = 1) +
  geom_smooth(method = "loess", span = 0.075, method.args = list(degree = 1), se = FALSE) + xlab("") +
  labs(caption = "Requiere mayor suavizamiento")
g_1 + g_2


## componente anual
mod_anual <- loess(res_1 ~ as.numeric(fecha), data = datos_dia, degree = 2, span = 0.005)
datos_dia <- datos_dia |> mutate(ajuste_2 = fitted(mod_anual)) |>
  mutate(res_2 = res_1 - ajuste_2)

ggplot(datos_dia, aes(x = fecha)) +
  geom_point(aes(y = res_1), alpha = 0.2, size = 1) +
  geom_line(aes(y = ajuste_2), colour = "red", size = 1.2)

## día de la semana
datos_dia <- datos_dia |>
  group_by(dia_semana) |>
  nest() |>
  mutate(ajuste_mod =
           map(data, ~ loess(res_2 ~ as.numeric(fecha), data = .x, span = 0.1, degree = 1))) |>
  mutate(ajuste_3 =  map(ajuste_mod, fitted)) |>
  select(-ajuste_mod) |>
  unnest(cols = c(data, ajuste_3)) |>
  mutate(res_3 = res_2 - ajuste_3) |>
  ungroup()

ggplot(datos_dia, aes(x = fecha)) +
  geom_point(aes(y = res_2), alpha = 0.2) +
  geom_line(aes(y = ajuste_3, colour = dia_semana), size = 1) +
  xlab("")

# residuales
ggplot(datos_dia, aes(x = fecha, y = res_3)) +
  geom_line() +
  geom_smooth(method = "loess", span = 0.02,
              method.args = list(degree=1, family = "symmetric"))
ggplot(datos_dia, aes(sample = res_3)) +
  geom_qq(distribution = stats::qunif) +
  ylab("Nacimientos (residual)") + xlab("")


# Gráfica de componentes
media <- mean(datos_dia$n) |> round()
datos_l <- datos_dia |>
  select(fecha, dia_semana, n, ajuste_1, ajuste_2, ajuste_3, res_3) |>
  mutate(ajuste_1_centrado = ajuste_1 - mean(ajuste_1)) |>
  gather(componente, valor, ajuste_2:ajuste_1_centrado) |>
  mutate(componente = recode(componente, ajuste_1_centrado = "Tendencia", 
                             ajuste_2 = "Anual", ajuste_3 = "Día de la semana",
                             res_3 = "Residual")) |>
  mutate(componente = fct_relevel(componente, "Tendencia", "Anual", "Día de la semana", "Residual"))
ggplot(datos_l, aes(x = fecha, y = valor, colour = dia_semana)) +
  facet_wrap(~ componente,  ncol = 1) +
  geom_point(size=0.5) + 
  labs(caption = "Media total: 6435")

