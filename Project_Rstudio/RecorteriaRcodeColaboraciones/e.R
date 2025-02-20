# Función exponencial e^(-x^2/2)
f <- function(x) exp(-x^2/2)

# Punto de expansión
x0 <- 0

# Orden de la serie (número de términos)
orden <- 100

# Fórmula de la serie de Taylor
taylor_serie <- function(x, n) {
  suma <- 0
  for (i in 0:n) {
    suma <- suma + (x - x0)^i / factorial(i)
  }
  return(suma)
}

# Calcular la serie de Taylor
orden_serie <- 5
serie_resultado <- taylor_serie(x = 2, n = orden_serie)

# Comparar con el valor real de e^2
valor_real <- f(2)

# Imprimir resultados
cat(paste("Serie de Taylor de orden", orden_serie, "para e^2:", serie_resultado, "\n"))
cat("Valor real de e^2:", valor_real, "\n")

