
# Limpieza y ordganizacion de la base de datos 

library(readxl)
library(ggplot2)
set.seed(1992)


SCA <- read_excel("TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/Base de datos COMPLETA 230822.xls", 
                  sheet = "GRUPO ENFERMOS")



Control <- read_excel("TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/Base de datos COMPLETA 230822.xls", 
                      sheet = "Grupo de Control")



Preclinicos <- read_excel("TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/Base de datos COMPLETA 230822.xls", 
                          sheet = "GRUPO PRECLINICOS")


# Contruir nueva Fila padecimiento en SCA
dato = replicate(nrow(SCA), "Enfermo")
SCA <- cbind(dato, SCA)
# Contruir nueva Fila padecimiento en Control
dato = replicate(nrow(Control), "Control")
Control <- cbind(dato, Control)
# Contruir nueva Fila padecimiento en Control
dato = replicate(nrow(Preclinicos), "Preclinico")
Preclinicos <- cbind(dato, Preclinicos)

Preclinicos <- Preclinicos[-1,]
SCA <- SCA[-1,]
Control <- Control[-1,]

write.csv(Control,file = "Documentos/TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_control.csv", row.names = FALSE)
write.csv(SCA,file = "Documentos/TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_sca.csv", row.names = FALSE)
write.csv(Preclinicos,file = "Documentos/TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_preclinicos.csv", row.names = FALSE)

############################# A partir de aqui 

set.seed(camilo)

Control = read.csv("/home/kamilo/Documentos/TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_control.csv", header=T)

SCA = read.csv("/home/kamilo/Documentos/TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_sca.csv", header=T)

Preclinicos = read.csv("/home/kamilo/Documentos/TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_preclinicos.csv", header=T)



# Analisis Exploratorio de los datos SCA

# dato observacion

# No numero de observacion es un id 

# Nombre y Apellido Variable identificativa 

names(SCA)
###################### var Sexo SCA
summary(SCA$Sexo)
SCA$Sexo <- factor(SCA$Sexo)
catg <- c("F","M")
fresexosca <- table(SCA$Sexo)
barplot(fresexosca, names.arg = catg, xlab = "Sexo en SCA", ylab = "Frecuencia")

##################### var Edad...4 edad que tiene durante el estudio

class(SCA$Edad...4)
summary(SCA$Edad...4)

#################### var Edad...5 edad con la que inicio la enfermedad

class(SCA$Edad...5)
SCA$Edad...5<- as.numeric(SCA$Edad...5)
summary(SCA$Edad...5)

##################### var Tiempo SCA 
summary(SCA$Tiempo)
SCA$Tiempo <- as.numeric(SCA$Tiempo)
summary(SCA$Tiempo)
SCA$Tiempo
table(SCA$Tiempo)
hist(SCA$Tiempo)


#################### var SARA escala de ataxia 

class(SCA$SARA)
table(SCA$SARA)

n <- length(SCA$SARA)
for (i in 1:n){
  if (SCA$SARA[i] == "Estadio ligero"){
    SCA$SARA[i] <- "Estadio Ligero"
  }
}
for (i in 1:n){
  if (SCA$SARA[i] == "Estadio severo cama"){
    SCA$SARA[i] <- "Estadio Severo Cama"
  }
}
SCA$SARA <- factor(SCA$SARA)
SCA$SARA <- ordered(SCA$SARA)

summary(SCA$SARA)
catg <- c("Estadio Ligero", "Estadio Moderado", "Estadio Severo Cama ")
val <- table(SCA$SARA)
barplot(val, names.arg = catg, xlab = "SARA en SCA", ylab = "Frecuencia")

#############################  Fenotipo

class(SCA$Fenotipo)

table(SCA$Fenotipo)

SCA$Fenotipo <- factor(SCA$Fenotipo)

summary(SCA$Fenotipo)

#################### var DAPMES SCA   Diámetro anteroposterior máximo perpendicular al eje del mesencéfalo
   
class(SCA$DAPMES)  
summary(SCA$DAPMES)

####################### DAPP 
class(SCA$DAPP)
summary(SCA$DAPP)  

####################### DAPM

class(SCA$DAPM)
summary(SCA$DAPM)

#################### var DAPCM Diámetro de los pedúnculos cerebelosos medios # Diámetro anteroposterior de canal medular en C2 (DAPCM). 

class(SCA$DAPCM)  
summary(SCA$DAPCM)


#################### CoM

class(SCA$`CoDM (DAPCM / DAPM )`)
summary(SCA$`CoDM (DAPCM / DAPM )`)


#################### DAPIV

class(SCA$DAPIV)
summary(SCA$DAPIV)

#################### var DHIV 

class(SCA$DHIV)  
summary(SCA$DHIV)

####################      DTIV

class(SCA$DTIV)  
summary(SCA$DTIV)


#####################  VIV

class(SCA$VIV) 
SCA$VIV <- as.numeric(SCA$VIV)
summary(SCA$VIV)

#####################  DAP
class(SCA$DAP)  
SCA$DAP <- as.numeric(SCA$DAP)
summary(SCA$DAP)

################### H

class(SCA$H) 
SCA$H <- as.numeric(SCA$H)
summary(SCA$H)

#########################  CAG Rango Alelo 1

SCA$`CAG Rango Alelo 1`

SCA$`CAG Rango Alelo 1` <- as.numeric(SCA$`CAG Rango Alelo 1`)
summary(SCA$`CAG Rango Alelo 1`)

CAGR_1 <- SCA$`CAG Rango Alelo 1`
CAGR_1[4] <- 22
CAGR_1[7] <- 24
CAGR_1[8] <- 22
CAGR_1[16] <- 22
CAGR_1[17] <- 22
CAGR_1[18] <- 22
CAGR_1[24] <- 35
CAGR_1[30] <- 22
CAGR_1[43] <- 26
CAGR_1[46] <- 22
CAGR_1[47] <- 22
CAGR_1[48] <- 22
CAGR_1[53] <- 22
CAGR_1[55] <- 22
CAGR_1[57] <- 24
CAGR_1[65] <- 22
SCA <- cbind(CAGR_1,SCA)
summary(CAGR_1)
SCA$CAGR_1 <- CAGR_1
##############################33

class(SCA$`CAG Rango Alelo 2`)
SCA$`CAG Rango Alelo 2` <- as.numeric(SCA$`CAG Rango Alelo 2`)
summary(SCA$`CAG Rango Alelo 2`)
CAGR_2 <- SCA$`CAG Rango Alelo 2`
CAGR_2[4] <- 37
CAGR_2[7] <- 35
CAGR_2[8] <- 45
CAGR_2[16] <- 37
CAGR_2[17] <- 39
CAGR_2[18] <- 38
CAGR_2[24] <- 35
CAGR_2[30] <- 33.5
CAGR_2[43] <- 36
CAGR_2[46] <- 36
CAGR_2[47] <- 34
CAGR_2[48] <- 36
CAGR_2[53] <- 44
CAGR_2[55] <- 35
CAGR_2[57] <- 35.5
CAGR_2[65] <- 49
SCA <- cbind(CAGR_2,SCA)

summary(SCA$CAGR_2)

#######################################

names(SCA) 

class(SCA$...22)

#Correlacionar estas variables 
columnas_numericas <- sapply(SCA, is.numeric)
SCA_numericas <- SCA[, columnas_numericas]

# Calcular la matriz de correlación
matriz_cor <- cor(SCA_numericas)

SCA_sin_NA <- na.omit(SCA_numericas)

matriz_corSINNA <- cor(SCA_sin_NA)

nrow(SCA_sin_NA)

nrow(SCA)

# Obtener los valores absolutos de la matriz de correlación
matriz_cor_abs <- abs(matriz_corSINNA)

# Obtener las variables más correlacionadas
variables_correlacionadas <- which(matriz_cor_abs > 0.7 & matriz_cor_abs < 1, arr.ind = TRUE)

# Imprimir las variables más correlacionadas
for (i in 1:nrow(variables_correlacionadas)) {
  var1 <- rownames(matriz_corSINNA)[variables_correlacionadas[i, 1]]
  var2 <- colnames(matriz_corSINNA)[variables_correlacionadas[i, 2]]
  correlacion <- matriz_corSINNA[variables_correlacionadas[i, 1], variables_correlacionadas[i, 2]]
  cat("Variables correlacionadas:", var1, "y", var2, "(", correlacion, ")\n")
}



# Diámetro anteroposterior de protuberancia (DAPP): diámetro anteroposterior máximo perpendicular al eje de protuberancia

#Diámetro anteroposterior de médula cervical en C2 (DAPM). 


####################################  Comparacion de Medias DAPMES - ANOVA

# Prueba de ANOVA
resultado_DAPMES <- aov(c(SCA$DAPMES, Preclinicos$DAPMES, Control$DAPMES) ~ rep(c("SCA DAPMES", "Preclinicos DAPMES", "Control DAPMES"), c(65,17,72)))

summary(resultado_DAPMES)

#### HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias DAPP - ANOVA

# Prueba de ANOVA
resultado_DAPP <- aov(c(SCA$DAPP, Preclinicos$DAPP, Control$DAPP) ~ rep(c("SCA DAPP", "Preclinicos DAPP", "Control DAPP"), c(65,17,72)))

summary(resultado_DAPP)

#### NO HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias DAPM - ANOVA

# Prueba de ANOVA
resultado_DAPM <- aov(c(SCA$DAPM, Preclinicos$DAPM, Control$DAPM) ~ rep(c("SCA DAPM", "Preclinicos DAPM", "Control DAPM"), c(65,17,72)))

summary(resultado_DAPM)

#### HAY DIFERENCIA SIGNIFICATIVA 


#################################### Comparacion de Medias DAPCM - ANOVA

# Prueba de ANOVA
resultado_DAPCM <- aov(c(SCA$DAPCM, Preclinicos$DAPCM, Control$DAPCM) ~ rep(c("SCA DAPCM", "Preclinicos DAPCM", "Control DAPCM"), c(65,17,72)))

summary(resultado_DAPCM)

#### NO HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias CoDM..DAPCM...DAPM.. - ANOVA

# Prueba de ANOVA
resultado_CoDM <- aov(c(SCA$CoDM..DAPCM...DAPM.., Preclinicos$CoDM..DAPCM...DAPM.., Control$CoDM..DAPCM...DAPM..) ~ rep(c("SCA CoDM", "Preclinicos CoDM", "Control CoDM"), c(65,17,72)))

summary(resultado_CoDM)

#### HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias DAPIV - ANOVA

# Prueba de ANOVA
resultado_DAPIV <- aov(c(SCA$DAPIV, Preclinicos$DAPIV, Control$DAPIV) ~ rep(c("SCA DAPIV", "Preclinicos DAPIV", "Control DAPIV"), c(65,17,72)))

summary(resultado_DAPIV)

#### HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias DHIV - ANOVA

# Prueba de ANOVA
resultado_DHIV <- aov(c(SCA$DHIV, Preclinicos$DHIV, Control$DHIV) ~ rep(c("SCA DHIV", "Preclinicos DHIV", "Control DHIV"), c(65,17,72)))

summary(resultado_DHIV)

#### HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias DTIV - ANOVA

# Prueba de ANOVA
resultado_DTIV <- aov(c(SCA$DTIV, Preclinicos$DTIV, Control$DTIV) ~ rep(c("SCA DTIV", "Preclinicos DTIV", "Control DTIV"), c(65,17,72)))

summary(resultado_DTIV)

#### HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias VIV - ANOVA

# Prueba de ANOVA
resultado_VIV <- aov(c(SCA$VIV, Preclinicos$VIV, Control$VIV) ~ rep(c("SCA VIV", "Preclinicos VIV", "Control VIV"), c(65,17,72)))

summary(resultado_VIV)

#### HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias VIV - ANOVA

# Prueba de ANOVA
resultado_VIV <- aov(c(SCA$VIV, Preclinicos$VIV, Control$VIV) ~ rep(c("SCA VIV", "Preclinicos VIV", "Control VIV"), c(65,17,72)))

summary(resultado_VIV)

#### HAY DIFERENCIA SIGNIFICATIVA 

#################################### Comparacion de Medias DAP - ANOVA

# Prueba de ANOVA
resultado_DAP <- aov(c(SCA$DAP, Preclinicos$DAP, Control$DAP) ~ rep(c("SCA DAP", "Preclinicos DAP", "Control DAP"), c(65,17,72)))

summary(resultado_DAP)

#### HAY DIFERENCIA SIGNIFICATIVA 


#################################### Comparacion de Medias H - ANOVA

# Prueba de ANOVA
resultado_H <- aov(c(SCA$H, Preclinicos$H, Control$H) ~ rep(c("SCA H", "Preclinicos H", "Control H"), c(65,17,72)))

summary(resultado_H)

#### HAY DIFERENCIA SIGNIFICATIVA 

################################## Conclusion sobre la comparación de medias 


#Variables utiles: H, DAP, VIV, DTIV, DHIV, DAPIV, CoDM, DAPM, DAPMES
#Variables que no son utiles: DAPCM, DAPP, 


# Preparar Datos Para Modelo Multinomial y Modelo Ordinal

############################################################################

# Unificar Datos para Obtener Muestra de Entrenamiento

id_SCA_l <- 1:nrow(SCA)

ID_test <- sample(id_SCA_l, 13, replace = F)
for (i in ID_test){
  SCA_l_test <- cbind(SCA[ID_test,])
  SCA_l_train <- SCA[-ID_test,]
}
SCA_l_test
SCA_l_train
id_Preclinicos_l <- 1:nrow(Preclinicos)
ID_test <- sample(id_Preclinicos, 4, replace = F)
for (i in ID_test){
  Preclinicos_l_test <- cbind(Preclinicos[ID_test,])
  Preclinicos_l_train <- Preclinicos[-ID_test,]
}
Preclinicos_l_test
Preclinicos_l_train

id_Control_l <- 1:nrow(Control)
ID_test <- sample(id_Control_l, 15, replace = F)
for (i in ID_test){
  Control_l_test <- cbind(Control[ID_test,])
  Control_l_train <- Control[-ID_test,]
}
Control_l_test
Control_l_train


dataset_train <- merge(SCA_l_train,Preclinicos_l_train, all = TRUE)
dataset_train <- merge(dataset_train,Control_l_train, all= TRUE)
dataset_train
dataset_test <- merge(SCA_l_test,Preclinicos_l_test, all = TRUE)
dataset_test <- merge(dataset_test,Control_l_test, all= TRUE)
dataset_test

################################# Modelo Multinomial
library(nnet)

modelo_multinomial <- multinom(dato ~ H + DAP + VIV + DTIV + DHIV + DAPIV + CoDM..DAPCM...DAPM.. + DAPM + DAPMES, data = dataset_train,  Hess = TRUE, model = TRUE)

summary(modelo_multinomial)


############################### Evaluar Modelo

# Paso 1: Predecir las clases en el conjunto de datos de prueba
predicciones <- predict(modelo_multinomial, newdata = dataset_test, type = "class")

# Paso 2: Comparar las predicciones con las clases reales
clases_reales <- dataset_test$dato

# Paso 3: Calcular la matriz de confusión
matriz_confusion <- table(predicciones, clases_reales)

# Paso 1: Obtener VP, FP, FN, VN de la matriz de confusión
VP <- matriz_confusion[1, 1]
FP <- matriz_confusion[2, 1] + matriz_confusion[3, 1]
FN <- matriz_confusion[1, 2] + matriz_confusion[1, 3]
VN <- sum(matriz_confusion) - (VP + FP + FN)

# Paso 2: Calcular sensibilidad, especificidad y precisión
sensibilidad <- VP / (VP + FN)
especificidad <- VN / (VN + FP)
precision <- VP / (VP + FP)

sensibilidad
especificidad
precision
############################################### Proceso de seleccion de variables 

library(MASS)

modelo_inicial_multi <- multinom(dato ~ H + DAP + VIV + DTIV + DHIV + DAPIV + CoDM..DAPCM...DAPM.. + DAPM + DAPMES, data = dataset_train,  Hess = TRUE, model = TRUE)

modelo_seleccionado <- stepAIC(modelo_inicial_multi, direction = "both")

modelo_final_multi <- update(modelo_inicial_multi, formula = formula(modelo_seleccionado))

summary(modelo_final_multi)
summary(modelo_multinomial)


anova(modelo_multinomial,modelo_final_multi)

##############################################


# Paso 1: Predecir las clases en el conjunto de datos de prueba
predicciones <- predict(modelo_final_multi, newdata = dataset_test, type = "class")

# Paso 2: Comparar las predicciones con las clases reales
clases_reales <- dataset_test$dato

# Paso 3: Calcular la matriz de confusión
matriz_confusion <- table(predicciones, clases_reales)

# Paso 1: Obtener VP, FP, FN, VN de la matriz de confusión
VP <- matriz_confusion[1, 1]
FP <- matriz_confusion[2, 1] + matriz_confusion[3, 1]
FN <- matriz_confusion[1, 2] + matriz_confusion[1, 3]
VN <- sum(matriz_confusion) - (VP + FP + FN)

# Paso 2: Calcular sensibilidad, especificidad y precisión
sensibilidad <- VP / (VP + FN)
especificidad <- VN / (VN + FP)
precision <- VP / (VP + FP)

sensibilidad
especificidad
precision




###################################################

#Datos Nuevos 
BD_Control <- read_excel("TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/BD Control y Preclinicos.xls", 
                                       sheet = "Grupo de Control")

BD_Preclinicos <- read_excel("TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/BD Control y Preclinicos.xls", 
                                       sheet = "GRUPO PRECLINICOS")

BD_SCA <- read_excel("TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/BD enfermos sin genetica.xls", 
                                       sheet = "GRUPO ENFERMOS")

dataset_SCA <- merge(SCA,BD_SCA, all = TRUE)
dataset_Control <- merge(Control,BD_Control, all = TRUE)
dataset_Preclinico <- merge(Preclinicos,BD_Preclinicos, all = TRUE)

write.csv(dataset_SCA,file = "TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_SCA_Final.csv", row.names = FALSE)
write.csv(dataset_Control,file = "TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_Control_Final.csv", row.names = FALSE)
write.csv(dataset_Preclinico,file = "TrabajoArticuloCuesta/Cuesta/procesamiento29abril2023/dataset_Preclinico_Final.csv", row.names = FALSE)

