# Función a expandir en serie de Taylor: e^(-x^2/2)
f <- function(x) exp(-x^2 / 2)

# Punto de expansión
x0 <- 0

# Orden de la serie (número de términos)
orden <- 5

# Fórmula de la serie de Taylor
taylor_serie <- function(x, n) {
  suma <- 0
  for (i in 0:n) {
    suma <- suma + (1 / factorial(i)) * (x - x0)^i
  }
  return(suma)
}

# Calcular la serie de Taylor
orden_serie <- 5
x_valor <- 1  # Valor en el que se evaluará la serie
serie_resultado <- taylor_serie(x = x_valor, n = orden_serie)

# Comparar con el valor real de e^(x^2/2)
valor_real <- f(x_valor)

# Imprimir resultados
cat(paste("Serie de Taylor de orden", orden_serie, "para e^(x^2/2) en x =", x_valor, ":", serie_resultado, "\n"))
cat("Valor real de e^(x^2/2) en x =", x_valor, ":", valor_real, "\n")

