#TODOS LOS EJEMPLOS DE TDAstats LIBRARY PROBADOS POR CAMILO MORA BATISTA
library(TDAstats)


#Example 1 

# get dataset (noisy circle) and calculate persistent homology
angles <- runif(100, 0, 2 * pi)
x <- cos(angles) + rnorm(100, mean = 0, sd = 0.1)
y <- sin(angles) + rnorm(100, mean = 0, sd = 0.1)
annulus <- cbind(x, y)
phom <- calculate_homology(annulus)
# find threshold of significance
# expecting 1 significant feature of dimension 1 (Betti-1 = 1 for annulus)
thresh <- id_significant(features = as.data.frame(phom),
                         dim = 1,
                         reps = 500,
                         cutoff = 0.975)
# generate flat persistence diagram
# every feature higher than `thresh` is significant
plot_persist(phom, flat = TRUE)

#Example2 

# create a 2-d point cloud of a circle (100 points)
num.pts <- 100
rand.angle <- runif(num.pts, 0, 2*pi)
pt.cloud <- cbind(cos(rand.angle), sin(rand.angle))
# calculate persistent homology (num.pts by 3 numeric matrix)
pers.hom <- calculate_homology(pt.cloud)
# plot calculated homology features as persistence diagram
plot_barcode(pers.hom)


#Example 3

# create a 2-d point cloud of a circle (100 points)
num.pts <- 100
rand.angle <- runif(num.pts, 0, 2*pi)
pt.cloud <- cbind(cos(rand.angle), sin(rand.angle))
# calculate persistent homology (num.pts by 3 numeric matrix)
pers.hom <- calculate_homology(pt.cloud)
# plot calculated homology features as persistence diagram
plot_persist(pers.hom)





#######################################333 MISMOS EJEMPLOS











# create a 2-d point cloud of a circle (100 points)
num.pts <- 100
rand.angle <- runif(num.pts, 0, 2*pi)
pt.cloud <- cbind(cos(rand.angle), sin(rand.angle))
# calculate persistent homology (num.pts by 3 numeric matrix)
pers.hom <- calculate_homology(pt.cloud)


#Examples
# get dataset (noisy circle) and calculate persistent homology
angles <- runif(100, 0, 2 * pi)
x <- cos(angles) + rnorm(100, mean = 0, sd = 0.1)
y <- sin(angles) + rnorm(100, mean = 0, sd = 0.1)
annulus <- cbind(x, y)
phom <- calculate_homology(annulus)
# find threshold of significance
# expecting 1 significant feature of dimension 1 (Betti-1 = 1 for annulus)
thresh <- id_significant(features = as.data.frame(phom),
                         dim = 1,
                         reps = 500,
                         cutoff = 0.975)
# generate flat persistence diagram
# every feature higher than `thresh` is significant
plot_persist(phom, flat = TRUE)

#Example
# create a 2-d point cloud of a circle (100 points)
num.pts <- 100
rand.angle <- runif(num.pts, 0, 2*pi)
pt.cloud <- cbind(cos(rand.angle), sin(rand.angle))
# calculate persistent homology (num.pts by 3 numeric matrix)
pers.hom <- calculate_homology(pt.cloud)
# plot calculated homology features as persistence diagram
plot_barcode(pers.hom)


#Examples
# create a 2-d point cloud of a circle (100 points)
num.pts <- 100
rand.angle <- runif(num.pts, 0, 2*pi)
pt.cloud <- cbind(cos(rand.angle), sin(rand.angle))
# calculate persistent homology (num.pts by 3 numeric matrix)
pers.hom <- calculate_homology(pt.cloud)
# plot calculated homology features as persistence diagram
plot_persist(pers.hom)

