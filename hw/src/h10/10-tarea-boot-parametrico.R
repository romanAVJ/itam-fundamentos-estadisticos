library(tidyverse)

#### Tarea Pasada: Funciones ####
# verosimilitud problema de errores
crear_log_verosim <- function(n, n_err){
  # n es tamaño de muestra 
  # n_err el número de errores detectados (datos)
  n_corr <- n - n_err
  log_verosim <- function(p){
    n_err * log(p) + n_corr * log(1-p)
  }
}

# crear verosimitud para n = 500, n_err = 121
log_v <- crear_log_verosim(n = 500, n_err = 121)

# encontrar max verosimilitud
optim <- optimize(log_v, interval = c(0, 1), maximum = TRUE)
optim

#############################
# Repite el ejercicio anterior si observas 317 errores
# log_v <- crear_log_verosim(n = 500, n_err = 317)

## Boostrap paramétrico
###################################################################
#### Ejercicio 2 ######
## Ahora calculamos error estándar para el ejercicio anterior
## usando bootstrap paramétrico


## calcula el error estándar de tu estimacion de max verosimilitud
# create function to find max verosimilitud for each simulation
rep_boot  <- function(logv, est_mle, n, B=250){
  # create parametric bootstrap
  xsim  <- rbinom(n = B, size = n, prob = est_mle)

  # find max verosimilitud for each simulation
  mle_sim  <- c()
  for (i in 1:B){
    log_v_sim  <- logv(n = n, n_err = xsim[i])
    optim_sim  <- optimize(log_v_sim, interval = c(0, 1), maximum = TRUE)
    mle_sim[i]  <- optim_sim$maximum
  }
  return(mle_sim)
}

# find standard error
reps_boot  <- rep_boot(logv = crear_log_verosim, est_mle = optim$maximum, n = 500, B = 250)

# plot histogram
ggplot(tibble(reps_boot), aes(x = reps_boot)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = optim$maximum, color = "red", size = 1.5) +
  labs(x = "Estimador de max verosimilitud", y = "Frecuencia") +
  ggtitle("Histograma de estimadores de max verosimilitud") +
  theme_bw()

# standard error
cat("El error estándar es:", sd(reps_boot), "\n")

## Paso 1 y 2: argumenta que el paso 1 y 2 ya lo hicimos arriba
## Pregunta: ¿cuál es tu estimador puntual?
cat("El estimador puntual es:", optim$maximum, "\n")

############################
# Paso 3: la siguiente función simula bajo nuestro modelo teórico
sim_modelo <- function(p, n){
  # rellena: qué es p y n
  muestra <- rbinom(n, 1, p)
  n_errores <- sum(muestra)
  n_errores
}

## investiga la función rbinom, ¿qué hace?
# R: la funcion rbionm genera una muestra de tamaño n de una distribución binomial

## calcula una simulación de estos datos, con alguna p y n fijas
xsim_model  <- sim_modelo(p = 0.5, n = 500)
cat("La simulación de estos datos es:", xsim_model, "\n")

##############################
## Paso 4: Enchufa tu estimador puntual de max verosimilitud
## y simula 3 mil observaciones
B  <- 3000
xsim_3000  <- rbinom(n = B, size = 500, prob = optim$maximum)

# grafica histograma
ggplot(tibble(xsim_3000), aes(x = xsim_3000)) +
  geom_histogram(bins = 20) +
  labs(x = "Simulaciones", y = "Frecuencia") +
  ggtitle("Histograma de simulaciones", "Bajo el estimador MV") +
  theme_bw()

#############################
## Paso 5: calcula el estimador de max verosimilitud para
## cada simulación
rep_boot  <- function(logv, est_mle, n, B=250){
  # create parametric bootstrap
  xsim  <- rbinom(n = B, size = n, prob = est_mle)

  # find max verosimilitud for each simulation
  mle_sim  <- c()
  for (i in 1:B){
    log_v_sim  <- logv(n = n, n_err = xsim[i])
    optim_sim  <- optimize(log_v_sim, interval = c(0, 1), maximum = TRUE)
    mle_sim[i]  <- optim_sim$maximum
  }
  return(mle_sim)
}

reps_boot  <- rep_boot(logv = crear_log_verosim, est_mle = optim$maximum, n = 500, B = 3000)

# grafica histograma
ggplot(tibble(reps_boot), aes(x = reps_boot)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = optim$maximum, color = "red", size = 1.5) +
  labs(x = "Estimador de max verosimilitud", y = "Frecuencia") +
  ggtitle("Histograma de estimadores de max verosimilitud") +
  theme_bw()

# descripcion de las simulaciones
cat("descripcion de las simulaciones:")
summary(reps_boot)

#############################
## Paso 6: resumen

## grafica un histograma de la distribución bootstrap

# ggplot()


## calcula el error estándar de tu estimacion de max verosimilitud


## tu codigo aquí


#####################################################################


