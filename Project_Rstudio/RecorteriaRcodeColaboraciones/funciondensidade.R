# Definir la función de densidad
f <- function(x) (1/sqrt(2*pi)) * exp(-x^2 / 2)

# Calcular la integral definida de -Inf a Inf
integral_result <- integrate(f, lower = -Inf, upper = Inf)

# Imprimir el resultado
print(integral_result)
