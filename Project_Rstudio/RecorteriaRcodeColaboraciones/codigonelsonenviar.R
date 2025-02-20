# Eliminar todos los objetos del entorno
rm(list=ls())

# Cargar librerías
library(readxl)  # para leer datos de Excel
library(car)     # para pruebas de diagnóstico y análisis de regresión

# Ruta del archivo Excel
archivo_excel <- "/home/kamilo/Descargas/tabla2.1.xlsx"

# Leer la hoja específica del archivo de Excel
datos <- read_excel(archivo_excel, sheet = "Hoja1")

# Analisis exploratorio
summary(datos)

head(datos)

# Skewness y Kurtosis
library(moments)
skewness(datos$res)     # Sesgo
kurtosis(datos$res)     # Kurtosis

# Boxplot y Histograma
boxplot(datos$res, col="deepskyblue2",  ylab="Temp")
hist(datos$res,  col="darkturquoise",  ylab="")
boxplot(datos$res, col="deeppink", ylab="lpres")
hist(datos$res, col="indianred2", ylab="")

# Gráfico de dispersión
plot(datos$res, datos$res)
rug(datos$edad, col = "red")

# Prueba de correlación
cor.test(datos$res, datos$edad, alternative = "two.sided")

# Ajuste del modelo lineal
modelo <- lm(res ~ edad, data = datos)

# Resumen del modelo
summary(modelo)

# Análisis de diagnóstico
par(mfrow=c(2,2))
plot(modelo)
dev.off()

# Pruebas de normalidad
shapiro.test (modelo$residuals)
ad.test(modelo$residuals)
cvm.test(modelo$residuals)
lillie.test(modelo$residuals)
pearson.test(modelo$residuals)
sf.test(modelo$residuals)
jarque.bera.test(modelo$residuals)

# Prueba de homocedasticidad
ncvTest(modelo)

# Pruebas de influencia
influencePlot(modelo)

