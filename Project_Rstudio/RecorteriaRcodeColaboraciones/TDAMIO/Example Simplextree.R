# Ejemplos de Simplex Tree Probados por Camlo Mora Batista
library(simplextree)
#Example 1

st <- simplex_tree(1:6)
st %>% adjacent(6)
# 1 3

adjacent(st, 6)

#Example 2

st <- simplex_tree()
st %>% insert(1:5)
print(st) ## Simplex Tree with (3, 3, 1) (0, 1, 2)-simplices

st %>% insert(9)
st %>% clear()
print(st) ## < empty simplex tree >

plot(st)

#Examples 3

st <- simplextree::simplex_tree(1:3)
st %>% print_simplices()
# 1, 2, 3, 1 2, 1 3, 2 3, 1 2 3
st %>% collapse(list(1:2, 1:3))
# 1, 2, 3, 1 3, 2 3=
st %>% insert(list(1:3, 2:5))
st %>% print_simplices("column")
# 1 2 3 4 5 1 1 2 2 2 3 3 4 1 2 2 2 3 2
# 2 3 3 4 5 4 5 5 2 3 3 4 4 3
# 3 4 5 5 5 4
# 5
st %>% collapse(list(2:4, 2:5))
st %>% print_simplices("column")
# 1 2 3 4 5 1 1 2 2 2 3 3 4 1 2 2 3
# 2 3 3 4 5 4 5 5 2 3 4 4
# 3 5 5 5



#Example 

st <- simplex_tree(1:3)
st %>% print_simplices()
# 1, 2, 3, 1 2, 1 3, 2 3, 1 2 3
st %>% contract(c(1, 3)) %>% print_simplices()
# 1, 2, 1 2

# Ejemplo varias funciones 

#Examples
st <- simplex_tree()
print(st$id_policy)
## "compressed"
st %>% generate_ids(3)
## 0 1 2
st %>% generate_ids(3)
## 0 1 2
st %>% insert(list(1,2,3))
print(st$vertices)
## 1 2 3
st %>% insert(as.list(st %>% generate_ids(2)))
st %>% print_simplices()
# 0, 1, 2, 3, 4
st %>% remove(4)
st %>% generate_ids(1)
# 4


#Example  Examples
st <- simplex_tree()
st %>% insert(1:3) ## inserts the 2-simplex { 1, 2, 3 }
st %>% insert(list(4:5, 6)) ## inserts a 1-simplex { 4, 5 } and a 0-simplex { 6 }.
st %>% insert(combn(5,3)) ## inserts all the 2-faces of a 4-simplex


#Example


st <- simplex_tree()
st %>% insert(1:3)
st %>% is_face(2:3, 1:3)
st %>% is_face(1:3, 2:3)


#Examples
st <- simplex_tree()
st %>% insert(list(1:2, 2:3))
st %>% is_tree() # true
st %>% insert(c(1, 3))
st %>% is_tree() # false

plot(st)

#Examples
library(simplextree)
all(nat_to_sub(seq(choose(100,2)), n = 100, k = 2) == combn(100,2))
## Generating pairwise combinadics is particularly fast
## Below: test to generate ~ 45k combinadics (note: better to use microbenchmark)
system.time({
  x <- seq(choose(300,2))
  nat_to_sub(x, n = 300, k = 2L)
})
## Compare with generating raw combinations
system.time(combn(300,2))


################## plot simplex

#Examples
## Simple 3-simplex
st <- simplex_tree() %>% insert(list(1:4))
## Default is categorical colors w/ diminishing opacity
plot(st)
## If supplied colors have alpha defined, use that
vpal <- rainbow(st$dimension + 1)
plot(st, color_pal = vpal)
## If alpha not supplied, decreasing opacity applied
plot(st, color_pal = substring(vpal, first=1, last=7))
## Bigger example; observe only maximal faces (+vertices and edges) are drawn
st <- simplex_tree(list(1:3, 2:5, 5:9, 7:8, 10))
plot(st, color_pal = rainbow(st$dimension + 1))
## If maximal == FALSE, every simplex is drawn (even on top of each other)
vpal <- rainbow(st$dimension + 1)[c(1,2,5,4,3)]
pal_alpha <- c(1, 1, 0.2, 0.35, 0.35)
vpal <- sapply(seq_along(vpal), function(i) adjustcolor(vpal[i], alpha.f = pal_alpha[i]))
plot(st, color_pal = vpal, maximal = FALSE)
## You can also color each simplex individually by supplying a vector
## of the same length as the number of simplices.
plot(st, color_pal = sample(rainbow(sum(st$n_simplices))))
## The order is assumed to follow the level order traversal (first 0-simplices, 1-, etc.)
## This example colors simplices on a rainbow gradient based on the sum of their labels
si_sum <- straverse(st %>% level_order, sum)
rbw_pal <- rev(rainbow(50, start=0,end=4/6))
plot(st, color_pal=rbw_pal[cut(si_sum, breaks=50, labels = FALSE)])
## This also makes highlighting simplicial operations fairly trivial
four_cofaces <- as.list(cofaces(st, 4))
coface_pal <- straverse(level_order(st), function(simplex){
  ifelse(list(simplex) %in% four_cofaces, "orange", "blue")
})
plot(st, color_pal=unlist(coface_pal))
## You can also give a named list to draw individual simplices.
## **Only the maximal simplices in the list are drawn**
blue_vertices <- structure(as.list(rep("blue", 5)), names=as.character(seq(5, 9)))


plot(st, color_pal=append(blue_vertices, list("5,6,7,8,9"="red")))

#Examples
st <- simplex_tree()
st %>% insert(1:3) %>% print_simplices("tree")
# 1 (h = 2): .( 2 3 )..( 3 )
# 2 (h = 1): .( 3 )
# 3 (h = 0):
st %>% reindex(4:6) %>% print_simplices("tree")
# 4 (h = 2): .( 5 6 )..( 6 )
# 5 (h = 1): .( 6 )
# 6 (h = 0):



#Examples
st <- simplex_tree(list(1:5, 7:9))
st2 <- deserialize(serialize(st))
all.equal(as.list(preorder(st)), as.list(preorder(st2)))
# TRUE
set.seed(1234)
R <- rips(dist(replicate(2, rnorm(100))), eps = pnorm(0.10), dim = 2)
print(R$n_simplices)
# 100 384 851
## Approx. size of the full complex
print(utils::object.size(as.list(preorder(R))), units = "Kb")
# 106.4 Kb
## Approx. size of serialized version
print(utils::object.size(serialize(R)), units = "Kb")
# 5.4 Kb
## You can save these to disk via e.g. saveRDS(serialize(R), ...)




#Examples
## Recreating simplex tree from figure.
st <- simplex_tree()
st %>% insert(list(1:3, 2:5, c(6, 7, 9), 7:8, 10))
plot(st)
## Example insertion
