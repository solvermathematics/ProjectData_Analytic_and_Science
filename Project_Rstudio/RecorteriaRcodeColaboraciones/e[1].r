# Define la función
f <- function(x) exp(-x^2/2)

# Punto en el que deseas expandir la serie (por ejemplo, x = 0)
x0 <- 0

# Orden de la serie (número de términos)
orden <- 5

# Calcula la expansión en serie de Taylor
serie <- series(f, x = 0, order = 4)

# Muestra la serie
print(serie)
