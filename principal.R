library('tidyverse')
library('stats')
set.seed(42)

# Coeficientes "platonicos" (i.e., del proceso generador de datos)
beta_pgd <- c(4, 2, -3, 0.5, 0)

generadores <- list(
    "x1" = function(n) { runif(n, min=-5, max=5) },
    "x2" = function(n) { runif(n, min=-5, max=5) },
    "x3" = function(n) { runif(n, min=-5, max=5) },
    "x4" = function(n) { runif(n, min=-5, max=5) },
    "eps" = function(n) { rexp(n, rate = 1/2) - 2 },
#    "eps2" = function(n) { rchisq(n, 3) - 3 },
    "y" = function(x1, x2, x3, x4, eps, ...) { c(1, x1, x2, x3, x4) %*% beta_pgd + eps }
)

generar_muestra <- function(n, generadores) {
  # Tibble vacio
  df <- tibble(.rows = n)
  # Genero variables regresoras y errores
  for (nombre in names(generadores)) {
    if (nombre != "y") {
      df[nombre] <- generadores[[nombre]](n)
    }
  }
  # Genero y
  df[["y"]] <- pmap_dbl(df, generadores[["y"]])
  
  return(df)
}

intervalo_conf <- function(a_vec, llamada_lm, alfa, metodo = "exacto") {
  
  betahat <- llamada_lm$coefficients
  # Matriz de covarianza estimada para los coeficientes
  Sigmahat <- vcov(llamada_lm)
  
  n_muestra <- nrow(llamada_lm$model)
  r <- llamada_lm$rank
  # Cualcul cuantil t o z, segun corresponda
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

n_test <- 6
muestra <- generar_muestra(n_test, generadores)
llamada_lm <- lm(y ~ x1 + x2 + x3 + x4, muestra)
alfa <- 0.10
a1 <- c(0,1,0,0,0)
ic1_ex <- intervalo_conf(a1, llamada_lm, alfa, metodo = "exacto")
cubre(ic1_ex, beta_pgd[2])
ic1_as <- intervalo_conf(a1, llamada_lm, alfa, metodo = "asintotico")
cubre(ic1_as, beta_pgd[2])
a4 <- c(0,0,0,0,1)
ic4_ex <- intervalo_conf(a4, llamada_lm, alfa, metodo = "exacto")
cubre(ic4_ex, beta_pgd[5])
ic4_as <- intervalo_conf(a4, llamada_lm, alfa, metodo = "asintotico")
cubre(ic4_as, beta_pgd[5])

