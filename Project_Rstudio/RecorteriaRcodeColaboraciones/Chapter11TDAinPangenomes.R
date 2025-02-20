
# Chapter 11 TDA in pangenomes 
# Camilo Mora Batista,  Tarea TDA python to R

# "Necesitamos leer el archivo mini-genomes.blast que produjimos en el episodio de Entendiendo los Pangenomas con BLAST."

url = "https://raw.githubusercontent.com/paumayell/pangenomics/gh-pages/files/mini-genomes.blast"

blastE <- read.table(url, header = FALSE, sep = '\t', col.names = c('qseqid', 'sseqid', 'evalue'))

# Obtener una lista de los genes únicos


# Extraer valores únicos de la columna 'qseqid'
qseqid_unique <- unique(blastE$qseqid)

# Extraer valores únicos de la columna 'sseqid'
sseqid_unique <- unique(blastE$sseqid)

# Combinar valores únicos de ambas columnas en una lista única de genes
genes <- unique(c(qseqid_unique, sseqid_unique))

# Tenemos 43 genes únicos, podemos verificarlo de la siguiente manera.

length(genes)

# También necesitaremos una lista de los genomas únicos en nuestra base de datos. Primero, convertimos a un objeto de marco de datos la lista de genes, luego dividimos cada gen en las partes de 'genoma' y 'gen', y finalmente obtenemos una lista de los genomas únicos y la guardamos en el objeto 'genomes'.

# Crear un DataFrame llamado df_genes
df_genes <- data.frame(Genes = genes)

# Dividir la columna 'Genes' en dos columnas usando el carácter '|'
df_genome_genes <- strsplit(df_genes$Genes, "|", fixed = TRUE)
df_genome_genes <- data.frame(matrix(unlist(df_genome_genes), ncol = 2, byrow = TRUE))
colnames(df_genome_genes) <- c("Genome", "Gen")

# Obtener los valores únicos de la columna 'Genome'
genomes <- unique(df_genome_genes$Genome)
genomes <- as.list(genomes)

# Para utilizar los paquetes de Gudhi, necesitamos una matriz de distancias. En este caso, usaremos el valor 'evalue' como medida de qué tan similares son los genes. Primero, procesaremos el marco de datos blastE a una lista y luego lo convertiremos en un objeto de matriz

# Seleccionar filas donde tanto 'qseqid' como 'sseqid' estén presentes en la lista de genes
distance_list <- blastE[blastE$qseqid %in% genes & blastE$sseqid %in% genes, ]

# Mostrar las primeras filas de distance_list
head(distance_list)

# Como vimos en el episodio "Understanding Pangenomes with BLAST", el valor E del BLAST representa la posibilidad de encontrar una coincidencia con una puntuación similar en una base de datos. Por defecto, BLAST considera una puntuación máxima para el valor E de 10, pero en este caso, hay coincidencias de baja calidad. Si dos secuencias no son similares o si el valor E es mayor que 10, entonces BLAST no guarda esta puntuación. Con el fin de tener algo similar a una matriz de distancias, llenaremos el valor E de la secuencia para la cual no tenemos una puntuación. Para hacer esto, utilizaremos la convención de que un valor E igual a 5 es demasiado grande y que las secuencias no son similares en absoluto.

# Establecer el valor MaxDistance
MaxDistance <- 5.0000000

############## a veces hay que resetear la seccion
# Reshape de largo a ancho
matrixE <- reshape2::dcast(distance_list, qseqid ~ sseqid, value.var = "evalue")

# Mostrar las primeras 4 filas y 4 columnas de la matriz
head(matrixE[, 2:5])

dim(matrixE)

# Llenar los valores faltantes con MaxDistance
matrixE2 <- matrixE
matrixE2[is.na(matrixE2)] <- MaxDistance

# Mostrar las primeras 4 filas y 4 columnas de la matriz
head(matrixE2[, 2:5])


# Necesitamos tener un objeto con los nombres de las columnas de la matriz que utilizaremos más tarde

# Obtener los nombres de las columnas de la matriz o marco de datos
name_columns <- colnames(matrixE2)

# Mostrar los nombres de las columnas
name_columns

# Finalmente, necesitamos la matriz de distancias como un arreglo de numpy pero en R

# Convertir el marco de datos a una matriz
DistanceMatrix <- as.matrix(matrixE2)

# Mostrar la matriz resultante
dim(DistanceMatrix)

Distancia <- DistanceMatrix[,-1]
dim(Distancia)

Distancia <- as.numeric(Distancia)

Distancia <- matrix(Distancia, nrow = 43)

#Distancia <- as.numeric(Distancia)

#Ahora, queremos construir el complejo Vietoris-Rips asociado con los genes con respecto a la matriz de distancias que obtuvimos. En el episodio de Análisis Topológico de Datos, vimos que para construir el complejo Vietoris-Rips necesitamos definir un parámetro de distancia o umbral, de modo que los puntos dentro de una distancia menor o igual al umbral se conecten en el complejo. El umbral se define mediante el argumento max_edge_length, y aquí usaremos el valor 2


VR <- ripsDiag(Distancia, maxdimension=2, maxscale=2, dist="arbitrary", library = c("Dionysus"), location = TRUE, printProgress = TRUE)



#Diagrama de persistencia y c?digo de barras de persistencia
par(mfrow=c(1,2))
plot(VR[["diagram"]], band=0.1)
plot(VR[["diagram"]], barcode=TRUE)


VR

###Notas del Capitulo

# Obtener el número de vértices
num_vertices <- nrow(VR$diagram)
print(paste("Número de vértices:", num_vertices))

# Obtener la dimensión del complejo
dimension <- max(VR$diagram[, "dimension"])
print(paste("Dimensión del complejo:", dimension))


# Obtener información sobre los símplices
simplices_info <- VR$simplices

# Imprimir el número de símplices en cada dimensión
for (dim in seq_along(simplices_info)) {
  num_simplices <- length(simplices_info[[dim]])
  cat("Número de símplices en dimensión", dim, ":", num_simplices, "\n")
}

num_simplices <- length(simplices_info[2])



#Librerias útiles
library(TDA)
library(ripserr)
library(ggtda)
library(simplextree)
library(tdaunif)
library(TDAstats)
library(rgudhi)
library(simplextree)



