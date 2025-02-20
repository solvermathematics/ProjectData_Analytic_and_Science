#Análisis Topológico de Datos (TDA)

#Descargar e instalar paquetes: "TDA", "Hmisc"
install.packages(c ("TDA", "Hmisc"))
library(TDA)
library(Hmisc)
library(ggplot2)

#Importar datos
EG<-read.table("ExpresionGenes.txt", header=TRUE, sep="\t", dec=".", row.names=1)
dim(EG)
EG[1:10, 1:10]
length(which(is.na(EG)))

#Analizar distribución de datos
EGn<-substr(colnames(EG), start=1, stop=3)
#Se construye el marco de datos (data frame)
EGdf<-data.frame(Celulas=rep(EGn, times=74), Genes=rep(rownames(EG), each=71),
	Expresion=as.numeric(t(EG)))
head(EGdf, 20)
EG[1:2,1:20]
dim(Edf)
#[1] 10878 3
#Gráfica de densidad
ggplot(EGdf, aes(x=Expresion, color=Celulas))+ geom_density(na.rm=TRUE)+
	geom_vline(aes(xintercept=mean(Expresion, na.rm=TRUE)), color="red",
			linetype="dashed", size=1)

#Dividir datos en base al tipo celular: E14tg2a y R1
grep("E14tg2a.", colnames(EG))
E14<-EG[,1:38]
R1<-EG[,39:71]

#Obtener matriz de correlaciones, método de Spearman
#Uso del paquete Hmisc
#Función rcorr hace la coorrelación por columnas
#Método de deleción pareada, en caso de valores no adecuados (NA)
E<-E14
E<-R1
E<-rcorr(t(E), type="spearman")
mode(E)
names(E)
#[1] "r" "n" "P"
#r: matriz de correlaciones
#n: matriz del número de datos tomados para obtener la correlación
#P: valores de significancia P
dim(E$r)
E$r[1:10, 1:10]
#Cuántos datos de correlación caen dentro de los ragos: (-1, -0.5) y (0.5, 1)?


#Calcular la matriz de distancia
D<-1 - abs(E$r)
dim(D)
D[1:10, 1:10]
length(which(D<0.5))

#Uso del paquete "TDA"
#Filtrado con método de Vietoris Rips
#2 funciones:
#1)ripsFiltration: genera lista con elementos:
#"cmplx": lista de simplex filtrados 
#"values": valores de e (radio), correspondiente a los simplex encontrados
#"increasing": indica si los valores de e aumentan a lo largo del filtrado

#2)ripsDiag: genera lista con elementos:
#"diagram": columna que indica dimensión, inicio y fin de los siplex encontrados
#"birthLocation" "deathLocation" "cycleLocation": localización de los parámetros indicados
VR <- ripsDiag(X = D, maxdimension=1, maxscale=1.2, dist="arbitrary", library = c("Dionysus"), location = TRUE, printProgress = TRUE)
VR$diagram[1:10,]

#Diagrama de persistencia y código de barras de persistencia
par(mfrow=c(1,2))
plot(VR[["diagram"]], band=0.1)
plot(VR[["diagram"]], barcode=TRUE)

#Hacer una función desde el paso de correlación
#Forma canónica
#Nombre<-function(varible1, variable 2), {función}
#Ejemplo:
TDAc<-function(E){
	if (!require(Hmisc)) library(Hmisc)
	E<-rcorr(t(E), type="spearman")
	D<-1 - abs(E$r)
	if (!require(TDA)) library(TDA)
	VR <- ripsDiag(X = D, maxdimension=1, maxscale=1.2, dist="arbitrary", library = c("Dionysus"))
	}
#Análisis (repetido) con datos E14
E<-E14
TDAc(E)
VRE14<-VR
#Análisis con datos R1
E<-R1
TDAc(E)
VRR1<-VR

#Vizualizar diagramas de persistencia de los dos tipos celulares
par(mfrow=c(1,2))
plot(VRE14[["diagram"]], band=0.1)
plot(VRR1[["diagram"]], band=0.1)
