library(readxl)
library(ggplot2)
set.seed(1992)


SCA <- read_excel("/home/camilo/Documentos/Doctorado en Matemática/TrabajoCruzCarreto/ArticuloMultinomialSCA2/Base de datos COMPLETA 230822.xls", 
                  sheet = "GRUPO ENFERMOS")



Control <- read_excel("/home/camilo/Documentos/Doctorado en Matemática/TrabajoCruzCarreto/ArticuloMultinomialSCA2/Base de datos COMPLETA 230822.xls", 
                      sheet = "Grupo de Control")



Preclinicos <- read_excel("/home/camilo/Documentos/Doctorado en Matemática/TrabajoCruzCarreto/ArticuloMultinomialSCA2/Base de datos COMPLETA 230822.xls", 
                          sheet = "GRUPO PRECLINICOS")


nrow(SCA)

# Contruir nueva Fila padecimiento en SCA
padecimiento = replicate(nrow(SCA), "E")
SCA <- cbind(padecimiento, SCA)
# Contruir nueva Fila padecimiento en Control
padecimiento = replicate(nrow(Control), "C")
Control <- cbind(padecimiento, Control)
# Contruir nueva Fila padecimiento en Control
padecimiento = replicate(nrow(Preclinicos), "P")
Preclinicos <- cbind(padecimiento, Preclinicos)

# Limpieza de los Datos para tomar los dataframe a estudiar 

SCA_l <- SCA[complete.cases(SCA$DAPMES),]
Control_l <- Control[complete.cases(Control$DAPMES),]
Preclinicos_l <- Preclinicos[complete.cases(Preclinicos$DAPMES),]

# Unificar Datos para Obtener Muestra de Entrenamiento

id_SCA_l <- 1:nrow(SCA_l)
ID_test <- sample(id_SCA_l, 13, replace = F)
for (i in ID_test){
  SCA_l_test <- cbind(SCA_l[ID_test,])
  SCA_l_train <- SCA_l[-ID_test,]
}
SCA_l_test
SCA_l_train
id_Preclinicos_l <- 1:nrow(Preclinicos_l)
ID_test <- sample(id_Preclinicos_l, 4, replace = F)
for (i in ID_test){
  Preclinicos_l_test <- cbind(Preclinicos_l[ID_test,])
  Preclinicos_l_train <- Preclinicos_l[-ID_test,]
}
Preclinicos_l_test
Preclinicos_l_train

id_Control_l <- 1:nrow(Control_l)
ID_test <- sample(id_Control_l, 14, replace = F)
for (i in ID_test){
  Control_l_test <- cbind(Control_l[ID_test,])
  Control_l_train <- Control_l[-ID_test,]
}
Control_l_test
Control_l_train


dataset_train <- merge(SCA_l_train,Preclinicos_l_train, all = TRUE)
dataset_train <- merge(dataset_train,Control_l_train, all= TRUE)
dataset_train
dataset_test <- merge(SCA_l_test,Preclinicos_l_test, all = TRUE)
dataset_test <- merge(dataset_test,Control_l_test, all= TRUE)
dataset_test

##################### Datos Numericos 

DAPMES <- as.numeric(dataset_train$DAPMES)
DAPP <- as.numeric(dataset_train$DAPP)
DAPM <- as.numeric(dataset_train$DAPM)
DAPCM <- as.numeric(dataset_train$DAPCM )
DAPIV <- as.numeric(dataset_train$DAPIV)
DAP <- as.numeric(dataset_train$DAP)



####################################### Analisis Exploratorio
DAPMES <- as.numeric(dataset_train$DAPMES)
padecimiento <- dataset_train$padecimiento
hist(as.numeric(dataset_train$DAPMES))

###########DAPMES

tapply(DAPMES,padecimiento, summary)
tapply(DAPMES,padecimiento, sd)
tapply(as.numeric(DAPMES),padecimiento, mean)
tapply(DAPMES,padecimiento, var)

boxplot(as.numeric(dataset_train$DAPMES) ~ padecimiento,xlab="Estado", ylab="DAPMES", data=dataset_train, col=2:9, main="")
points(DAPMES ~ padecimiento)


##########################  

tapply(DAPMES,padecimiento, summary)
tapply(DAPMES,padecimiento, sd)
tapply(as.numeric(DAPMES),padecimiento, mean)
tapply(DAPMES,padecimiento, var)

boxplot(as.numeric(dataset_train$DAPMES) ~ padecimiento,xlab="Estado", ylab="DAPMES", data=dataset_train, col=2:9, main="")
points(DAPMES ~ padecimiento)


######################################## Modelo de Entrenamiento 
#library(MASS)
#modelo_glm <- glm(padecimiento ~ DAPMES + DAPP + DAPM + DAPCM + DAPIV + DAP, data = dataset_train, family = multinomial(link = "logit"))
#library(nnet)
#modelo_glm <- multinom(padecimiento ~ DAPMES + DAPP + DAPM + DAPCM + DAPIV + DAP, data = dataset_train)
#####################################
# ECUACIONES 

# En un modelo de regresión multinomial como este, la relación entre las variables independientes y la variable dependiente se modela mediante una función logit. El modelo asigna una probabilidad a cada una de las categorías de la variable dependiente (en este caso, E y P), en función de los valores de las variables independientes.

# La ecuación que representa este modelo es la siguiente:
  
#  Logit(P(padecimiento=E|DAPMES, DAPP, DAPM)) = b0 + b1DAPMES + b2DAPP + b3*DAPM

#  Logit(P(padecimiento=P|DAPMES, DAPP, DAPM)) = a0 + a1DAPMES + a2DAPP + a3*DAPM

#  donde b0, b1, b2, y b3 son los coeficientes de regresión para la categoría de la variable dependiente E, a0, a1, a2 y a3 son los coeficientes de regresión para la categoría P, y DAPMES, DAPP y DAPM son las variables independientes.


library(nnet)

# Crear un modelo lineal generalizado multinomial
modelo_glm <- multinom(padecimiento ~ as.numeric(DAPMES) + as.numeric(DAPP) + as.numeric(DAPM), data = dataset_train)


#variables 
# DAPMES Diámetro anteroposterior del mesencéfalo (DAPMES)
# Diámetro anteroposterior de protuberancia (DAPP): diámetro anteroposterior máximo perpendicular al eje de protuberancia.
# Diámetro anteroposterior de médula cervical a nivel de  C2 (DAPM).
# Mostrar el resumen del modelo
summary(modelo_glm)

###################################+
#+ Este es un modelo de regresión multinomial que intenta predecir el valor de la variable categórica "padecimiento" en función de tres variables numéricas: DAPMES, DAPP y DAPM. La salida del modelo muestra los coeficientes de regresión para cada variable independiente, así como el intercepto para cada uno de los dos valores posibles de la variable dependiente (E y P).
# El intercepto y los coeficientes indican la dirección y la magnitud de la relación entre cada variable independiente y la variable dependiente. Por ejemplo, el valor del intercepto para E (42.96209) indica el valor esperado de la variable dependiente para un caso donde todas las variables independientes son cero.

# Los coeficientes negativos para DAPMES, DAPP y DAPM indican que a medida que aumentan los valores de estas variables independientes, la probabilidad de que la variable dependiente sea P disminuye. Los valores de los coeficientes indican la cantidad en que la probabilidad de la variable dependiente disminuye por cada unidad de aumento en la variable independiente correspondiente.
# Los errores estándar asociados con cada coeficiente se utilizan para determinar la significancia estadística de los coeficientes. Un error estándar menor indica una mayor precisión en la estimación del coeficiente.
# El Residual Deviance es una medida de cuán bien se ajusta el modelo a los datos y se utiliza para comparar diferentes modelos. Un valor más bajo indica un mejor ajuste. El AIC (Criterio de Información de Akaike) es una medida de la calidad del modelo que tiene en cuenta tanto la precisión del ajuste como la complejidad del modelo. Un valor más bajo indica un mejor modelo en términos de balance entre precisión y complejidad. 


#+ ################################### Predicción
#+ 
#+ 
predictions <- predict(modelo_glm, newdata = dataset_test, type = "class")
table(predictions, dataset_test$padecimiento)


# Sensibilidad (Clase C) = VP / (VP + FN) = 13 / (13 + 1) = 0.93
# Sensibilidad (Clase E) = VP / (VP + FN) = 13 / (13 + 2) = 0.87
# Sensibilidad (Clase P) = VP / (VP + FN) = 0 / (0 + 0) = NA

# Especificidad (Clase C) = VN / (VN + FP) = (13 + 13) / (13 + 13 + 2) = 0.87
# Especificidad (Clase E) = VN / (VN + FP) = (13 + 11) / (11 + 1 + 13) = 0.92
# Especificidad (Clase P) = VN / (VN + FP) = (0 + 14) / (14 + 3) = 0.82

################################################################################ Seleccion de Variable 

library(MASS)

start_model <- multinom(padecimiento ~ as.numeric(DAPMES) + as.numeric(DAPP) + as.numeric(DAPM), data = dataset_train) #glm(Species ~ 1, data = train_data, family = "multinomial")
final_model <- stepAIC(start_model, direction = "forward", scope = list(lower = ~1, upper = ~ as.numeric(DAPMES) + as.numeric(DAPP) + as.numeric(DAPM)), data = dataset_train)
summary(final_model)

# Se optiene el mismo modelo

################################################################################ Curve ROC
library(pROC)
#roc_curve <- roc(dataset_test$padecimiento, predictions, levels=c("C", "E", "P"))
roc_curve_E <- roc(ifelse(dataset_test$padecimiento == "E", 1, 0), 
                 ifelse(predictions == "E", 1, 0))


plot(roc_curve_E, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1,0.2), grid.col="gray")

roc_curve_P <- roc(ifelse(dataset_test$padecimiento == "P", 1, 0), 
                 ifelse(predictions == "P", 1, 0))

plot(roc_curve_P, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1,0.2), grid.col="blue")

roc_curve_C <- roc(ifelse(dataset_test$padecimiento == "C", 1, 0), 
                   ifelse(predictions == "C", 1, 0))

plot(roc_curve_C, print.auc=TRUE, auc.polygon=TRUE, grid=c(0.1,0.2), grid.col="red")

#SacarLosValoresCortes y ver si se pueden ordenar 




length(predictions)
length(dataset_test$padecimiento)

attach(dataset_train)

padecimiento <- factor(padecimiento)

Modelo_pade <- lm(DAPMES ~ padecimiento)

summary(Modelo_pade)

plot(Modelo_pade, residuals = TRUE)




#detach(Control_l)
############################################# Volumen bajo la Curva 

library(VUROCS)

VUS(dataset_test$padecimiento,predictions)

VUSvar(dataset_test$padecimiento,predictions) # 


is.numeric(dataset_test$padecimiento)

class(dataset_test$padecimiento)

VUS(dataset_test$padecimiento,predictions)
clar(dataset_test$padecimiento,predictions)
clarAdj(dataset_test$padecimiento,predictions)
SomersD(dataset_test$padecimiento,predictions)
Kruskal_Gamma(dataset_test$padecimiento,predictions)
Kendall_taub(dataset_test$padecimiento,predictions)
Kendall_tauc(dataset_test$padecimiento,predictions)
VUSvar(dataset_test$padecimiento,predictions)  # este solo funciona con valores nuemricos 
VUScov(dataset_test$padecimiento,predictions)  # este solo funciona con valores nuemricos 



############### example


y <- rep(1:5,each=3)
fx <- c(3,3,3,rep(2:5,each=3))
VUS(y,fx)
clar(y,fx)
clarAdj(y,fx)
SomersD(y,fx)
Kruskal_Gamma(y,fx)
Kendall_taub(y,fx)
Kendall_tauc(y,fx)
VUSvar(rep(1:5,each=3),c(1,2,3,rep(2:5,each=3)))
VUScov(c(1,2,1,3,2,3),c(1,2,3,4,5,6),c(1,3,2,4,6,5))
##########################################################################

# Modelo
modelo_test <- function(DAPMES,DAPP,DAPM) {
  probE <- exp(47.50222-1.517799*DAPMES-0.5803559*DAPP-1.2407461*DAPM)/(1+exp(47.50222-1.517799*DAPMES-0.5803559*DAPP-1.2407461*DAPM))
  probP <- exp(27.50783-1.040852*DAPMES-0.4737392*DAPP+0.1307908*DAPM)/(1+exp(27.50783-1.040852*DAPMES-0.4737392*DAPP+0.1307908*DAPM))
  probC <- 1-probE
  resultado <- c(probE,probP,probC)
  return(resultado)
}

modelo_test(as.numeric(dataset_test$DAPMES[3]),as.numeric(dataset_test$DAPP[3]), as.numeric(dataset_test$DAPM[3]))


k <- length(dataset_test$padecimiento)
predic_for <- c()
probabilidad_pred <- c()
for (i in 1:k) {
  # Código a ejecutar en cada iteración del bucle
  prob <- modelo_test(as.numeric(dataset_test$DAPMES[i]),as.numeric(dataset_test$DAPP[i]), as.numeric(dataset_test$DAPM[i]))
  probabilidad_predicha <- max(prob)
  indice_de_proba <- which.max(prob)
  if (indice_de_proba == 1) {
    predic_for[i] <- "E"
    probabilidad_pred[i] <- probabilidad_predicha
  }
  if (indice_de_proba == 2) {
      predic_for[i] <- "P"
      probabilidad_pred[i] <- probabilidad_predicha
  } 
  if (indice_de_proba == 3) {
        predic_for[i] <- "C"
        probabilidad_pred[i] <- probabilidad_predicha
  }

}
# Vector de ejemplo
vector <- c(10, 5, 8, 12, 6)

# Encontrar el valor máximo
maximo <- max(vector)

# Encontrar el índice del valor máximo
indice_maximo <- which.max(vector)




summary(predictions)



#####################################################Code Basura 
# Preprocessing of data for a simple regression. Make data_frame
SCA_l <- cbind(id=1:nrow(SCA_l), SCA_l)
Preclinicos_l <- cbind(id=(nrow(SCA_l)+1):(nrow(SCA_l)+nrow(Preclinicos_l)), Preclinicos_l)
Control_l <- cbind(id=(nrow(SCA_l)+nrow(Preclinicos_l)+1):(nrow(SCA_l)+nrow(Preclinicos_l)+nrow(Control_l)), Control_l)

datasetpart1 <- merge(SCA_l,Preclinicos_l, all = TRUE)
datasetfull <- merge(datasetpart1, Control_l, all = TRUE)

nrow(datasetfull)

SCA_l[39,]
datasetfull[2,]


X <- data.frame(padecimiento = replicate(65, 1),
                DAPMES = DAPMES_SCA)
X
Y <- data.frame(padecimiento = replicate(39, 0),
                DAPMES = DAPMES_Control)
Y

id_SCA <- 1:65
ID_test <- sample(id_SCA, 13, replace = F)

for (i in ID_test){
  X_test <- cbind(X[ID_test,])
  X_train <- X[-ID_test,]
}
X_test
X_train 

id_Control <- 1:39
ID_test <- sample(id_Control, 8, replace = F)
for (i in ID_test){
  Y_test <- cbind(Y[ID_test,])
  Y_train <- Y[-ID_test,]
}

Y_test
Y_train

Z_train <- merge(X_train,Y_train, all = TRUE)
Z_test <- merge(X_test,Y_test, all = TRUE)

Z_train



################## experimento

library(pROC)

# Datos de ejemplo
# Supongamos que tenemos probabilidades predichas para tres clases: "clase1", "clase2" y "clase3"
pred_clase1 <- c(0.8, 0.6, 0.3, 0.9)
pred_clase2 <- c(0.2, 0.4, 0.7, 0.1)
pred_clase3 <- c(0.1, 0.5, 0.2, 0.6)

# Etiquetas reales (clases verdaderas)
etiquetas_reales <- factor(c("clase1", "clase2", "clase3"))

# Combinar las probabilidades predichas para todas las clases
#probabilidades_predichas <- as.vetor(c(pred_clase1, pred_clase2, pred_clase3)
probabilidades_predichas <- c(pred_clase1, pred_clase2, pred_clase3)

etiquetas <- factor(etiquetas_reales, levels = unique(etiquetas_reales))

# Calcular la curva ROC
roc_obj <- multiclass.roc(etiquetas, probabilidades_predichas)
roc_obj <- multiclass.roc(etiquetas, probabilidades_predichas)

# Graficar la curva ROC
plot(roc_obj, print.thres = "best", main = "Curva ROC para un modelo de tres clases")


###############3 corrregidaaaa dice 

# Datos de ejemplo
# Supongamos que tenemos probabilidades predichas para tres clases: "clase1", "clase2" y "clase3"
pred_clase1 <- c(0.8, 0.6, 0.3, 0.9)
pred_clase2 <- c(0.2, 0.4, 0.7, 0.1)
pred_clase3 <- c(0.1, 0.5, 0.2, 0.6)

# Etiquetas reales (clases verdaderas)
etiquetas_reales <- factor(c("clase1", "clase2", "clase3","clase1", "clase2", "clase3","clase1", "clase2", "clase3","clase1", "clase2", "clase3"))

# Combinar las probabilidades predichas para todas las clases
probabilidades_predichas <- c(pred_clase1, pred_clase2, pred_clase3)

# Verificar la longitud de los vectores
if (length(etiquetas_reales) != length(probabilidades_predichas)) {
  stop("Los vectores de etiquetas y probabilidades predichas deben tener la misma longitud.")
}

# Crear un factor con las etiquetas y niveles únicos
etiquetas <- factor(etiquetas_reales, levels = unique(etiquetas_reales))

# Calcular la curva ROC
roc_obj <- multiclass.roc(etiquetas, probabilidades_predichas)

# Graficar la curva ROC
plot(roc_obj, print.auc = TRUE, print.auc.y = 0.5, print.auc.x = 0.5,
     print.thres = "best", main = "Curva ROC para un modelo de tres clases")



