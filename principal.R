library('tidyverse')
library('stats')
library('future')
library('furrr')
set.seed(42)

#setwd("/mnt/Datos/")

# Coeficientes "platonicos" (i.e., del proceso generador de datos)
beta_pgd <- c(4, 2, -3, 0.5, 0)

# Funciones generadoras de x_i
generadores_x <- list(
    "x1" = function(n) { runif(n, min=-5, max=5) },
    "x2" = function(n) { runif(n, min=-5, max=5) },
    "x3" = function(n) { runif(n, min=-5, max=5) },
    "x4" = function(n) { runif(n, min=-5, max=5) }
)

generadores_eps <- list(
  "normal" = function(n) { rnorm(n) },
  "exponencial" = function(n) { rexp(n, rate = 1/2) - 2 },
  "lognormal" = function(n) { exp(rnorm(n) - exp(0.5))  },
  "uniforme" = function(n) { runif(n, -3, 3) },
  "chi_cuadrado" = function(n) { rchisq(n, 3) - 3 },
  "student1" = function(n) { rt(n, 1) },
  "student3" = function(n) { rt(n, 3) }
)

generador_y <- function(x1, x2, x3, x4, beta_pgd, eps, ...)   {
  c(1, x1, x2, x3, x4) %*% beta_pgd + eps
}

generar_muestra <- function(n, generadores_x, generador_eps, beta_pgd) {
  # Tibble vacio
  df <- tibble(.rows = n)
  # Genero variables regresoras y errores
  for (nombre in names(generadores_x)) {
    if (nombre != "y") {
      df[nombre] <- generadores_x[[nombre]](n)
    }
  df$eps <- generador_eps(n)
  }
  # Genero y
  df["y"] <- pmap_dbl(df, generador_y, beta_pgd=beta_pgd)

  return(df)
}

intervalo_conf <- function(a_vec, llamada_lm, alfa, metodo = "exacto") {

  betahat <- llamada_lm$coefficients
  # Matriz de covarianza estimada para los coeficientes
  Sigmahat <- vcov(llamada_lm)

  n_muestra <- nrow(llamada_lm$model)
  r <- llamada_lm$rank
  # Cualculo cuantil t o z, segun corresponda
  if (metodo == "exacto") {
    cuantil <- qt(p = 1 - alfa/2, df = n_muestra - r)
  } else if (metodo == "asintotico") {
    cuantil <- qnorm(p = 1 - alfa/2)
  } else {
    stop("Los unicos metodos soportados son 'exacto' y 'asintotico'")
  }

  centro <- t(a_vec)%*%betahat
  delta <- cuantil * sqrt(t(a_vec) %*% Sigmahat %*% a_vec)
  return(c(centro - delta, centro + delta))
}

cubre <- function(intervalo, valor) { intervalo[1] <= valor & intervalo[2] >= valor}


ayudante_generar_muestra <- function(distr_eps, generadores_x, beta_pgd, n) {
  generar_muestra(n,generadores_x, generadores_eps[[distr_eps]],beta_pgd=beta_pgd)
}

#Pasamos a modo multihilo porque se vienen cálculos feos

future.globals.maxSize = '+inf'


#n_muestrales <- c(10, 25, 100)
n_muestrales <- c(10, 25, 100, 250, 500, 1000, 1500, 2000, 3000)
## max_n_muestral <- max(n_muestrales)
## n_sims <- 1000
## muestras_maestras <- crossing(
##   n_sim = seq(max_n_muestral),
##   distr_eps = names(generadores_eps)) %>%
##   mutate(
##     muestra = future_map(.progress=TRUE,
##                   distr_eps,
##                   ayudante_generar_muestra,
##                   generadores_x = generadores_x,
##                   beta_pgd = beta_pgd,
##                   n = max_n_muestral)
##   )

#muestras_maestras %>% write_rds("muestras_maestras.Rds")


muestras_maestras <- read_rds("muestras_maestras.Rds")

# El '-3' es poco legible, buscar cómo sustraer una columna por nombre.

muestras_puntuales <- muestras_maestras[-3] %>%
  crossing(
    n = n_muestrales
  )


muestras_puntuales %>% write_rds("muestras_puntuales.Rds")

## ayudante_intervalo_conf <- function(fun_a, llamada_lm, met_int, alfa) {
##  intervalo_conf(a_vec = funciones_a[[fun_a]], llamada_lm, metodo = met_int, alfa)
## }

#ayudante recibe el número de simulación, n y la distribución de epsilon. En base a eso elabora un intervalo con el método met_int de nivel 1- alfa.

ayudante_intervalo_conf <- function(n_simulacion, distr_epsilon, n, fun_a, met_int, alfa) {
  muestra_a_evaluar <- (muestras_maestras %>% filter(n_sim==n_simulacion,distr_eps==distr_epsilon))[[1,'muestra']] %>% head(n)
  modelo <- lm(y ~ x1 + x2 + x3 +x4,data=muestra_a_evaluar)
  intervalo_conf(a_vec = funciones_a[[fun_a]], llamada_lm=modelo, alfa=alfa, metodo = met_int)
}


#  Combinaciones lineales de beta_pgd a estimar (matriz A q*p de la teoría general).
funciones_a <- list(
  beta1 = c(0, 1, 0, 0, 0),
  beta4 = c(0, 0, 0, 0, 1)
)

metodos_intervalo <- c("asintotico", "exacto")
alfa <- 0.1
## intervalos <- muestras_puntuales %>%
##   crossing(
##     fun_a = names(funciones_a),
##     met_int = metodos_intervalo) %>%
##   mutate(
##     #atbeta es el valor del parámetro en el PGD.
##     atbeta = map_dbl(fun_a, function(i) funciones_a[[i]] %*% beta_pgd),
##     ic = future_pmap( .progress = TRUE,
##       list(n_sim, distr_eps, n, fun_a, met_int),
##       ayudante_intervalo_conf,
##       alfa = alfa),
##     cubre = map2_lgl(ic, atbeta, cubre),
##     ic_low = map_dbl(ic, 1),
##     ic_upp = map_dbl(ic, 2)
##     )

# Guardamos la simulación

#intervalos %>% write_rds("simulacion.Rds")

intervalos <- read_rds("simulacion.csv")

# Esto lo hice para probar que diera algo. Y da!
sintesis <- intervalos %>%
  group_by(distr_eps, n, met_int, fun_a) %>%
  summarise(prop_cubre = mean(cubre)) %>% write_rds("sintesis-resultados.csv")
