# Crear datos
data1 <- list(
  'Genoma1' = c(0, 0),
  'Genoma2' = c(1, 0),
  'Genoma3' = c(0, 1),
  'Genoma4' = c(1, 1)
)

# Crear el DataFrame
df_1 <- data.frame(data1, row.names = c('Gen1', 'Gen2'))

# Mostrar el DataFrame
print(df_1)


distance <- function(df) {
  # Compute pairwise distances between columns of the DataFrame
  distances <- dist(t(df))
  
  # Convert the distance matrix to a squareform distance matrix
  distance_matrix <- as.matrix(distances)
  return(distance_matrix)
}

# Ejemplo de uso
data <- data.frame(
  'Genoma1' = c(0, 1, 0, 1),
  'Genoma2' = c(0, 0, 1, 1)
)

result <- distance(data)
print(result)




CVR <- ripsDiag(result, maxdimension=2, maxscale=2, dist="arbitrary", library = c("Dionysus"), location = TRUE, printProgress = TRUE)



##Diagrama de persistencia y c?digo de barras de persistencia


par(mfrow=c(1,2))
plot(CVR[["diagram"]], band=0.5)
plot(CVR[["diagram"]], barcode=TRUE)

CVR


