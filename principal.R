library('tidyverse')
library('stats')
set.seed(42)


generadores <- list(
    "x1" = function(n) { runif(n, min=-5, max=5) },
    "x2" = function(n) { runif(n, min=-5, max=5) },
    "x3" = function(n) { runif(n, min=-5, max=5) },
    "x4" = function(n) { runif(n, min=-5, max=5) },
    "eps" = function(n) { rexp(n, rate = 1/2) - 2 },
#    "eps2" = function(n) { rchisq(n, 3) - 3 },
    "y" = function(x1, x2, x3, x4, eps, ...) { 4 + 2*x1 - 3*x2 + 0.5*x3 + 0*x4 + eps }
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

n_test <- 10
muestra <- generar_muestra(n_test, generadores)
llamada_lm <- lm(y ~ x1 + x2 + x3 + x4, muestra)

vcov(llamada_lm)

intervalo_conf <- function(a_vec, llamada_lm, alfa, metodo = "exacto") {
  
  betahat <- llamada_lm$coefficients
  # Matrix de covarianza estimada para los coeficientes
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

alfa <- 0.05
a_vec <- c(0,0,0,0,1)
intervalo_conf(a_vec, llamada_lm, alfa, metodo = "exacto")
