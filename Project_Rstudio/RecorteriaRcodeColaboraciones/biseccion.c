#include <stdio.h>
#include <math.h>

#define EPSILON 0.00001 // Tolerancia para la convergencia
#define MAX_ITER 100    // Máximo número de iteraciones

// Definición de la función f(x)
double f(double x) {
  return x * x * x - 4 * x * x + 3 * x + 1;
}

// Implementación del método de la bisección
double bisection(double a, double b) {
  double c;
  
  for (int i = 0; i < MAX_ITER; i++) {
    c = (a + b) / 2;
    
    if (f(c) == 0 || fabs(b - a) < EPSILON) {
      return c; // Raíz encontrada o convergencia
    }
    
    if (f(c) * f(a) < 0) {
      b = c;
    } else {
      a = c;
    }
  }
  
  return c;
}

int main() {
  double a = -10.0; // Límite izquierdo del intervalo inicial
  double b = 10.0;  // Límite derecho del intervalo inicial
  
  double root = bisection(a, b);
  
  printf("La raíz aproximada es: %lf\n", root);
  
  return 0;
}