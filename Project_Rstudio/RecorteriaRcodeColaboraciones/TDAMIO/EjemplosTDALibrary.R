#TODOS LOS EJEMPLOS DE TDA LIBRARY PROBADOS POR CAMILO MORA BATISTA
library(TDA)

#Example 1 de AlphaComplex

# input data generated from a circle
X <- circleUnif(n = 30)
# persistence diagram of alpha complex
DiagAlphaCmplx <- alphaComplexDiag(
  X = X, library = c("GUDHI", "Dionysus"), location = TRUE,
  printProgress = TRUE)
# plot
par(mfrow = c(1, 2))
plot(DiagAlphaCmplx[["diagram"]])
one <- which(DiagAlphaCmplx[["diagram"]][, 1] == 1)
one <- one[which.max(
  DiagAlphaCmplx[["diagram"]][one, 3] - DiagAlphaCmplx[["diagram"]][one, 2])]
plot(X, col = 2, main = "Representative loop of data points")
for (i in seq(along = one)) {
  for (j in seq_len(dim(DiagAlphaCmplx[["cycleLocation"]][[one[i]]])[1])) {
    lines(
      DiagAlphaCmplx[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1,
      col = i)
  }
}
par(mfrow = c(1, 1))


#Example2 Alpha Complex Filtrations

# input data generated from a circle
X <- circleUnif(n = 10)
plot(x)
# alpha complex filtration
FltAlphaComplex <- alphaComplexFiltration(X = X, printProgress = TRUE)
# plot alpha complex filtration
lim <- rep(c(-1, 1), 2)
plot(NULL, type = "n", xlim = lim[1:2], ylim = lim[3:4],
     main = "Alpha Complex Filtration Plot")
for (idx in seq(along = FltAlphaComplex[["cmplx"]])) {
  polygon(FltAlphaComplex[["coordinates"]][FltAlphaComplex[["cmplx"]][[idx]], , drop = FALSE],
          col = "pink", border = NA, xlim = lim[1:2], ylim = lim[3:4])
}
for (idx in seq(along = FltAlphaComplex[["cmplx"]])) {
  polygon(FltAlphaComplex[["coordinates"]][FltAlphaComplex[["cmplx"]][[idx]], , drop = FALSE],
          col = NULL, xlim = lim[1:2], ylim = lim[3:4])
}
points(FltAlphaComplex[["coordinates"]], pch = 16)


# Diagrama de Pesistencia de Alpha Shape Complex

# input data generated from cylinder
n <- 30
X <- cbind(circleUnif(n = n), runif(n = n, min = -0.1, max = 0.1))
# persistence diagram of alpha shape
DiagAlphaShape <- alphaShapeDiag(
  X = X, maxdimension = 1, library = c("GUDHI", "Dionysus"), location = TRUE,
  printProgress = TRUE)
# plot diagram and first two dimension of data
par(mfrow = c(1, 2))
plot(DiagAlphaShape[["diagram"]])
plot(X[, 1:2], col = 2, main = "Representative loop of alpha shape filtration")
one <- which(DiagAlphaShape[["diagram"]][, 1] == 1)
one <- one[which.max(
  DiagAlphaShape[["diagram"]][one, 3] - DiagAlphaShape[["diagram"]][one, 2])]
for (i in seq(along = one)) {
  for (j in seq_len(dim(DiagAlphaShape[["cycleLocation"]][[one[i]]])[1])) {
    lines(
      DiagAlphaShape[["cycleLocation"]][[one[i]]][j, , 1:2], pch = 19,
      cex = 1, col = i)
  }
}
par(mfrow = c(1, 1))


#Example 3 Alpha Sahpe in 3d

# input data generated from sphere
X <- sphereUnif(n = 20, d = 2)
# alpha shape filtration
FltAlphaShape <- alphaShapeFiltration(X = X, printProgress = TRUE)


#Example

# Generate data from mixture of 2 normals.
n <- 2000
X <- c(rnorm(n / 2), rnorm(n / 2, mean = 3, sd = 1.2))
# Construct a grid of points over which we evaluate the function
by <- 0.02
Grid <- seq(-3, 6, by = by)
## bandwidth for kernel density estimator
h <- 0.3
## Bootstrap confidence band
band <- bootstrapBand(X, kde, Grid, B = 80, parallel = FALSE, alpha = 0.05,
                      h = h)
plot(Grid, band[["fun"]], type = "l", lwd = 2,
     ylim = c(0, max(band[["band"]])), main = "kde with 0.95 confidence band")
lines(Grid, pmax(band[["band"]][, 1], 0), col = 2, lwd = 2)
lines(Grid, band[["band"]][, 2], col = 2, lwd = 2)


####Example

## confidence set for the Kernel Density Diagram
# input data
n <- 400
XX <- circleUnif(n)
## Ranges of the grid
Xlim <- c(-1.8, 1.8)
Ylim <- c(-1.6, 1.6)
lim <- cbind(Xlim, Ylim)
by <- 0.05
h <- .3 #bandwidth for the function kde
#Kernel Density Diagram of the superlevel sets
Diag <- gridDiag(XX, kde, lim = lim, by = by, sublevel = FALSE,
                 printProgress = TRUE, h = h)
# confidence set
B <- 10 ## the number of bootstrap iterations should be higher!
## this is just an example
alpha <- 0.05

cc <- bootstrapDiagram(XX, kde, lim = lim, by = by, sublevel = FALSE, B = B,
                       alpha = alpha, dimension = 1, printProgress = TRUE, h = h)
plot(Diag[["diagram"]], band = 2 * cc)


#Example

XX1 <- circleUnif(20)
XX2 <- circleUnif(20, r = 0.2)
DiagLim <- 5
maxdimension <- 1
Diag1 <- ripsDiag(XX1, maxdimension, DiagLim, printProgress = FALSE)
Diag2 <- ripsDiag(XX2, maxdimension, DiagLim, printProgress = FALSE)
bottleneckDist <- bottleneck(Diag1[["diagram"]], Diag2[["diagram"]],
                             dimension = 1)
print(bottleneckDist)





# Examples
X <- circleUnif(100)
plot(X)



#plot.clusterTree
#Examples
## Generate data: 3 clusters
n <- 1200 #sample size
Neach <- floor(n / 4)
X1 <- cbind(rnorm(Neach, 1, .8), rnorm(Neach, 5, 0.8))
X2 <- cbind(rnorm(Neach, 3.5, .8), rnorm(Neach, 5, 0.8))
X3 <- cbind(rnorm(Neach, 6, 1), rnorm(Neach, 1, 1))
X <- rbind(X1, X2, X3)
k <- 100 #parameter of knn
## Density clustering using knn and kde
Tree <- clusterTree(X, k, density = "knn")
TreeKDE <- clusterTree(X, k, h = 0.3, density = "kde")
par(mfrow = c(2, 3))
plot(X, pch = 19, cex = 0.6)
# plot lambda trees
plot(Tree, type = "lambda", main = "lambda Tree (knn)")
plot(TreeKDE, type = "lambda", main = "lambda Tree (kde)")
# plot clusters
plot(X, pch = 19, cex = 0.6, main = "cluster labels")
for (i in Tree[["id"]]){
  points(matrix(X[Tree[["DataPoints"]][[i]],],ncol = 2), col = i, pch = 19,
         cex = 0.6)
}
#plot kappa trees
plot(Tree, type = "kappa", main = "kappa Tree (knn)")
plot(TreeKDE, type = "kappa", main = "kappa Tree (kde)")



# Example 
# Generate Data from the unit circle
n <- 300
X <- circleUnif(n)
## Construct a grid of points over which we evaluate the function
interval <- 0.065
Xseq <- seq(-1.6, 1.6, by = interval)
Yseq <- seq(-1.7, 1.7, by = interval)
Grid <- expand.grid(Xseq, Yseq)
## distance fct
distance <- distFct(X, Grid)


#Example

## Generate Data from the unit circle
n <- 300
X <- circleUnif(n)
## Construct a grid of points over which we evaluate the function
by <- 0.065
Xseq <- seq(-1.6, 1.6, by = by)
Yseq <- seq(-1.7, 1.7, by = by)
Grid <- expand.grid(Xseq, Yseq)
## distance to measure
m0 <- 0.1
DTM <- dtm(X, Grid, m0)

# Example
n <- 5
X <- cbind(cos(2*pi*seq_len(n)/n), sin(2*pi*seq_len(n)/n))
maxdimension <- 1
maxscale <- 1.5
dist <- "euclidean"
library <- "Dionysus"
FltRips <- ripsFiltration(X = X, maxdimension = maxdimension,
                          maxscale = maxscale, dist = "euclidean", library = "Dionysus",
                          printProgress = TRUE)
DiagFltRips <- filtrationDiag(filtration = FltRips, maxdimension = maxdimension,
                              library = "Dionysus", location = TRUE, printProgress = TRUE)
plot(DiagFltRips[["diagram"]])                    


#Examples
n <- 5
X <- cbind(cos(2*pi*seq_len(n)/n), sin(2*pi*seq_len(n)/n))
maxdimension <- 1
maxscale <- 1.5
dist <- "euclidean"
library <- "Dionysus"
FltRips <- ripsFiltration(X = X, maxdimension = maxdimension,
                          maxscale = maxscale, dist = "euclidean", library = "Dionysus",
                          printProgress = TRUE)
FUNvalues <- X[, 1] + X[, 2]
FltFun <- funFiltration(FUNvalues = FUNvalues, cmplx = FltRips[["cmplx"]])



#Example


## Distance Function Diagram and Kernel Density Diagram
# input data
n <- 300
XX <- circleUnif(n)
## Ranges of the grid
Xlim <- c(-1.8, 1.8)
Ylim <- c(-1.6, 1.6)
lim <- cbind(Xlim, Ylim)
by <- 0.05
h <- .3 #bandwidth for the function kde
#Distance Function Diagram of the sublevel sets
Diag1 <- gridDiag(XX, distFct, lim = lim, by = by, sublevel = TRUE,
                  printProgress = TRUE)
#Kernel Density Diagram of the superlevel sets
Diag2 <- gridDiag(XX, kde, lim = lim, by = by, sublevel = FALSE,
                  library = "Dionysus", location = TRUE, printProgress = TRUE, h = h)
#plot
#par(mfrow = c(2, 2))
plot(XX, cex = 0.5, pch = 19)
title(main = "Data")
plot(Diag1[["diagram"]])
title(main = "Distance Function Diagram")
plot(Diag2[["diagram"]])
title(main = "Density Persistence Diagram")
one <- which(Diag2[["diagram"]][, 1] == 1)
plot(XX, col = 2, main = "Representative loop of grid points")

for (i in seq(along = one)) {
  points(Diag2[["birthLocation"]][one[i], , drop = FALSE], pch = 15, cex = 3,
         col = i)
  points(Diag2[["deathLocation"]][one[i], , drop = FALSE], pch = 17, cex = 3,
         col = i)
  for (j in seq_len(dim(Diag2[["cycleLocation"]][[one[i]]])[1])) {
    lines(Diag2[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1, col = i)
  }
}


#Example

# input data
n <- 10
XX <- circleUnif(n)
## Ranges of the grid
Xlim <- c(-1, 1)
Ylim <- c(-1, 1)
lim <- cbind(Xlim, Ylim)
by <- 1

#Distance Function Diagram of the sublevel sets
FltGrid <- gridFiltration(
  XX, distFct, lim = lim, by = by, sublevel = TRUE, printProgress = TRUE)

#Example

X <- circleUnif(1000)
interval <- hausdInterval(X, m = 800)
print(interval)

# Example

## Generate Data from the unit circle
n <- 300
X <- circleUnif(n)
## Construct a grid of points over which we evaluate the function
by <- 0.065
Xseq <- seq(-1.6, 1.6, by=by)
Yseq <- seq(-1.7, 1.7, by=by)
Grid <- expand.grid(Xseq,Yseq)
## kernel density estimator
h <- 0.3
KDE <- kde(X, Grid, h)


#Examples
## Generate Data from the unit circle
n <- 300
X <- circleUnif(n)
## Construct a grid of points over which we evaluate the functions
by <- 0.065
Xseq <- seq(-1.6, 1.6, by = by)
Yseq <- seq(-1.7, 1.7, by = by)
Grid <- expand.grid(Xseq, Yseq)
## kernel distance estimator
h <- 0.3
Kdist <- kernelDist(X, Grid, h)


#Examples
## Generate Data from the unit circle
n <- 300
X <- circleUnif(n)
## Construct a grid of points over which we evaluate the function
by <- 0.065
Xseq <- seq(-1.6, 1.6, by = by)
Yseq <- seq(-1.7, 1.7, by = by)
Grid <- expand.grid(Xseq, Yseq)
## kernel density estimator
k <- 50
KNN <- knnDE(X, Grid, k)
KNN <- knnDE(X, Grid, k)

#Examples
Diag <- matrix(c(0, 0, 10, 1, 0, 3, 1, 3, 8), ncol = 3, byrow = TRUE)
DiagLim <- 10
colnames(Diag) <- c("dimension", "Birth", "Death")
#persistence landscape
tseq <- seq(0,DiagLim, length = 1000)
Land <- landscape(Diag, dimension = 1, KK = 1, tseq)
par(mfrow = c(1,2))
plot.diagram(Diag)
plot(tseq, Land, type = "l", xlab = "t", ylab = "landscape", asp = 1)


# Example ## input data: circle with clutter noise
n <- 600
percNoise <- 0.1
XX1 <- circleUnif(n)
noise <- cbind(runif(percNoise * n, -2, 2), runif(percNoise * n, -2, 2))
X <- rbind(XX1, noise)
## limits of the Gird at which the density estimator is evaluated
Xlim <- c(-2, 2)
Ylim <- c(-2, 2)
lim <- cbind(Xlim, Ylim)
by <- 0.2
B <- 80
alpha <- 0.05
## candidates

parametersKDE <- seq(0.1, 0.5, by = 0.2)
maxKDE <- maxPersistence(kde, parametersKDE, X, lim = lim, by = by,
                         bandFUN = "bootstrapBand", B = B, alpha = alpha,
                         parallel = FALSE, printProgress = TRUE)
print(summary(maxKDE))
par(mfrow = c(1,2))
plot(X, pch = 16, cex = 0.5, main = "Circle")
plot(maxKDE)


###Examples
nn <- 3000 #large sample size
mm <- 50 #small subsample size
NN <- 5 #we will compute NN diagrams using subsamples of size mm
XX <- circleUnif(nn) ## large sample from the unit circle
DiagLim <- 2
maxdimension <- 1
tseq <- seq(0, DiagLim, length = 1000)
Diags <- list() #here we will store the NN rips diagrams
#constructed using different subsamples of mm points
#here we'll store the landscapes
Lands <- matrix(0, nrow = NN, ncol = length(tseq))
for (i in seq_len(NN)){
  subXX <- XX[sample(seq_len(nn), mm), ]
  Diags[[i]] <- ripsDiag(subXX, maxdimension, DiagLim)
  Lands[i, ] <- landscape(Diags[[i]][["diagram"]], dimension = 1, KK = 1, tseq)
}
## now we use the NN landscapes to construct a confidence band
B <- 50
alpha <- 0.05
boot <- multipBootstrap(Lands, B, alpha)
LOWband <- boot[["band"]][, 1]
UPband <- boot[["band"]][, 2]
MeanLand <- boot[["mean"]]
plot(tseq, MeanLand, type = "l", lwd = 2, xlab = "", ylab = "",
     main = "Mean Landscape with band", ylim = c(0, 1.2))
polygon(c(tseq, rev(tseq)), c(LOWband, rev(UPband)), col = "pink")
lines(tseq, MeanLand, lwd = 1, col = 2)


# Example

## Generate data: 3 clusters
n <- 1200 #sample size
Neach <- floor(n / 4)
X1 <- cbind(rnorm(Neach, 1, .8), rnorm(Neach, 5, 0.8))
X2 <- cbind(rnorm(Neach, 3.5, .8), rnorm(Neach, 5, 0.8))
X3 <- cbind(rnorm(Neach, 6, 1), rnorm(Neach, 1, 1))
XX <- rbind(X1, X2, X3)
k <- 100 #parameter of knn
## Density clustering using knn and kde
Tree <- clusterTree(XX, k, density = "knn")
TreeKDE <- clusterTree(XX,k, h = 0.3, density = "kde")
par(mfrow = c(2, 3))
plot(XX, pch = 19, cex = 0.6)
# plot lambda trees
plot(Tree, type = "lambda", main = "lambda Tree (knn)")
plot(TreeKDE, type = "lambda", main = "lambda Tree (kde)")
# plot clusters
plot(XX, pch = 19, cex = 0.6, main = "cluster labels")
for (i in Tree[["id"]]){
  points(matrix(XX[Tree[["DataPoints"]][[i]], ], ncol = 2), col = i, pch = 19,
         cex = 0.6)
}
#plot kappa trees
plot(Tree, type = "kappa", main = "kappa Tree (knn)")
plot(TreeKDE, type = "kappa", main = "kappa Tree (kde)")


#Examples
XX1 <- circleUnif(30)
XX2 <- circleUnif(30, r = 2) + 3
XX <- rbind(XX1, XX2)
DiagLim <- 5
maxdimension <- 1
## rips diagram
Diag <- ripsDiag(XX, maxdimension, DiagLim, printProgress = TRUE)
#plot
par(mfrow = c(1, 3))
plot(Diag[["diagram"]])
plot(Diag[["diagram"]], rotated = TRUE)
plot(Diag[["diagram"]], barcode = TRUE)

#Examples
## input data: circle with clutter noise
n <- 600
percNoise <- 0.1
XX1 <- circleUnif(n)
noise <- cbind(runif(percNoise * n, -2, 2), runif(percNoise * n, -2, 2))
X <- rbind(XX1, noise)
## limits of the Gird at which the density estimator is evaluated
Xlim <- c(-2, 2)
Ylim <- c(-2, 2)
lim <- cbind(Xlim, Ylim)
by <- 0.2
B <- 80
alpha <- 0.05
## candidates
parametersKDE <- seq(0.1, 0.5, by = 0.2)
maxKDE <- maxPersistence(kde, parametersKDE, X, lim = lim, by = by,
                         bandFUN = "bootstrapBand", B = B, alpha = alpha,
                         parallel = FALSE, printProgress = TRUE)
print(summary(maxKDE))
par(mfrow = c(1, 2))
plot(X, pch = 16, cex = 0.5, main = "Circle")
plot(maxKDE)

#Examples  CREOO ESTE ES EL MAS IMPORTANTE
## EXAMPLE 1: rips diagram for circles (euclidean distance)
X <- circleUnif(30)
maxscale <- 5
maxdimension <- 1
## note that the input X is a point cloud
DiagRips <- ripsDiag(
  X = X, maxdimension = maxdimension, maxscale = maxscale,
  library = "Dionysus", location = TRUE, printProgress = TRUE)
# plot
layout(matrix(c(1, 3, 2, 2), 2, 2))
plot(X, cex = 0.5, pch = 19)
title(main = "Data")
plot(DiagRips[["diagram"]])
title(main = "rips Diagram")
one <- which(
  DiagRips[["diagram"]][, 1] == 1 &
    DiagRips[["diagram"]][, 3] - DiagRips[["diagram"]][, 2] > 0.5)
plot(X, col = 2, main = "Representative loop of data points")
for (i in seq(along = one)) {
  for (j in seq_len(dim(DiagRips[["cycleLocation"]][[one[i]]])[1])) {
    lines(
      DiagRips[["cycleLocation"]][[one[i]]][j, , ], pch = 19, cex = 1,
      col = i)
  }
}
## EXAMPLE 2: rips diagram with arbitrary distance
## distance matrix for triangle with edges of length: 1,2,4
distX <- matrix(c(0, 1, 2, 1, 0, 4, 2, 4, 0), ncol = 3)
maxscale <- 5
maxdimension <- 1
## note that the input distXX is a distance matrix
DiagTri <- ripsDiag(distX, maxdimension, maxscale, dist = "arbitrary",
                    printProgress = TRUE)
#points with lifetime = 0 are not shown. e.g. the loop of the triangle.
print(DiagTri[["diagram"]])


#Examples  RIPS FILTYARTIONS
n <- 5
X <- cbind(cos(2*pi*seq_len(n)/n), sin(2*pi*seq_len(n)/n))
maxdimension <- 1
maxscale <- 1.5
FltRips <- ripsFiltration(X = X, maxdimension = maxdimension,
                          maxscale = maxscale, dist = "euclidean", library = "GUDHI",
                          printProgress = TRUE)
# plot rips filtration   NO FUNCIONAAAA
lim <- rep(c(-1, 1), 2)
plot(NULL, type = "n", xlim = lim[1:2], ylim = lim[3:4],
     main = "Rips Filtration Plot")
for (idx in seq(along = FltRips[["cmplx"]])) {
  polygon(FltRips[["coordinates"]][FltRips[["cmplx"]][[idx]], , drop = FALSE],
          col = "pink", border = NA, xlim = lim[1:2], ylim = lim[3:4])
}
for (idx in seq(along = FltRips[["cmplx"]])) {
  polygon(FltRips[["coordinates"]][FltRips[["cmplx"]][[idx]], , drop = FALSE],
          col = NULL, xlim = lim[1:2], ylim = lim[3:4])
}
points(FltRips[["coordinates"]], pch = 16)

#Example

Diag <- matrix(c(0, 0, 10, 1, 0, 3, 1, 3, 8), ncol = 3, byrow = TRUE)
DiagLim <- 10
colnames(Diag) <- c("dimension", "Birth", "Death")
#persistence silhouette
tseq <- seq(0, DiagLim, length = 1000)
Sil <- silhouette(Diag, p = 1, dimension = 1, tseq)
par(mfrow = c(1, 2))
plot.diagram(Diag)
plot(tseq, Sil, type = "l", xlab = "t", ylab = "silhouette", asp = 1)

#Examples
# Generate data from 2 circles
XX1 <- circleUnif(30)
XX2 <- circleUnif(30, r = 2) + 3
XX <- rbind(XX1, XX2)
DiagLim <- 5 # limit of the filtration
maxdimension <- 1 # computes betti0 and betti1
Diag <- ripsDiag(XX, maxdimension, DiagLim, printProgress = TRUE)
print(Diag[["diagram"]])
print(summary(Diag[["diagram"]]))


#Examples
XX1 <- circleUnif(20)
XX2 <- circleUnif(20, r = 0.2)
DiagLim <- 5
maxdimension <- 1
Diag1 <- ripsDiag(XX1, maxdimension, DiagLim, printProgress = FALSE)
Diag2 <- ripsDiag(XX2, maxdimension, DiagLim, printProgress = FALSE)
wassersteinDist <- wasserstein(Diag1[["diagram"]], Diag2[["diagram"]], p = 1,
                               dimension = 1)
print(wassersteinDist)
