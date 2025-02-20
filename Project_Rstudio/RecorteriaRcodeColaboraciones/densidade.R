# Definir la función de densidad
f <- function(x) (1/sqrt(2*pi)) * exp(-x^2 / 2)

# Calcular la integral definida de -Inf a Inf
integral_result <- integrate(f, lower = -Inf, upper = Inf)

# Imprimir el resultado
cat("El valor de la integral es:", integral_result$value, "\n")
