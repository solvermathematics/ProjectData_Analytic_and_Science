# Definir la función de densidad
f <- function(x) (1/sqrt(2*pi)) * exp(-x^2 / 2)

# Punto de expansión
x0 <- 0

# Orden de la serie (número de términos)
orden <- 100

# Calcular la serie de Taylor
taylor_serie <- function(x, n) {
  suma <- 0
  for (i in 0:n) {
    derivada_i <- (-1)^i * (x - x0)^(2*i) / (factorial(i) * (2^i))
    suma <- suma + derivada_i
  }
  return(suma)
}

# Valores de x para los que calcular la serie
x_valores <- seq(-3, 3, by = 0.1)

# Calcular y mostrar la serie de Taylor para diferentes valores de x
for (x_valor in x_valores) {
  serie_resultado <- taylor_serie(x = x_valor, n = orden)
  cat(paste("Serie de Taylor de orden", orden, "en x =", x_valor, ":", serie_resultado, "\n"))
}
