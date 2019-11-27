library(pacman)
p_load("vegan", "reshape", "foreach")
#################################################
# PCA
#################################################
irisPCA <- prcomp(iris[, 1:4])
summary(irisPCA)
plot(irisPCA)
biplot(irisPCA)
str(irisPCA)
# rotations are actually loadings
irisPCA$rotation
# x is matrix with data point values converted to new axis
pcaSummary <- summary(irisPCA)$importance %>% as.data.frame()
pcaSummary$names <- rownames(pcaSummary)
pcaSummary %>%
    reshape::melt() %>%
    filter(names != "Standard deviation") %>%
    ggplot(aes(variable, value, color = names, group = names)) +
        geom_point() +
        geom_line() +
        ylab("% of variance") +
        xlab("principal component")

data.frame(irisPCA$x, Species = iris$Species) %>%
    ggplot(., aes(PC1, PC2, color = Species)) +
        geom_point()
data.frame(irisPCA$x, Species = iris$Species) %>%
    ggplot(., aes(PC2, PC3, color = Species)) +
        geom_point()

#################################################
# MDS
#################################################
d <- dist(iris[, -5]) # euclidean distances between the rows
fit <- cmdscale(d, eig = TRUE, k = 2) # k is the number of dim

# plot solution
plotdt <- fit$points %>% as.data.frame()
names(plotdt) <- paste0("MDS", 1:2)
plotdt$names <- rownames(plotdt)
plotdt$Species <- iris$Species
ggplot(plotdt, aes(MDS1, MDS2, color = Species)) +
    geom_point(alpha = 0.5) 

#################################################
# nMDS
#################################################
d <- dist(iris[, -5]) # euclidean distances between the rows
fit <- vegan::metaMDS(d, k = 2) # k is the number of reduced dimensions
stressplot(fit)
plot(fit)
str(fit)
plotdata <- data.frame(fit$points, Species = iris$Species)
ggplot(plotdata, aes(MDS1, MDS2, color = Species)) +
    geom_point()

fit <- metaMDS(iris[, -5],
          distance = "bray",
          k = 3,
          maxit = 999, 
          trymax = 500)
stressplot(fit)
plotdata <- data.frame(fit$points, Species = iris$Species)
ggplot(plotdata, aes(MDS1, MDS2, color = Species)) +
    geom_point()


#################################################
# kmeans clustering
#################################################
res <- kmeans(iris[, 1:4], centers = 3, iter.max = 1000)
data.frame(iris, cluster = res$cluster) %>%
    ggplot(aes(Petal.Length, Petal.Width, color = as.factor(cluster))) +
        geom_point()
table(res$cluster, iris$Species)

# finding K
searchKres <- foreach( i = 1:8, .combine = rbind) %do% {
    res <- kmeans(iris[, 1:4], centers = i, iter.max = 1000)
    data.frame(k = i, tot.withinss = res$tot.withinss)
}
plot(searchKres, type = "b")

# try k=2
res <- kmeans(iris[, 1:4], centers = 2, iter.max = 1000)
data.frame(iris, cluster = res$cluster) %>%
    ggplot(aes(Petal.Length, Petal.Width, color = as.factor(cluster))) +
        geom_point()
table(res$cluster, iris$Species)


# what if I use PCA before?
iriskmeanspca <- kmeans(irisPCA$x[, 1:2], 3, 1000)
data.frame(iris, cluster = iriskmeanspca$cluster) %>%
    ggplot(aes(Petal.Length, Petal.Width, color = as.factor(cluster))) +
        geom_point()
table(iriskmeanspca$cluster, iris$Species)
# very similar... so last 2 PC really do not hold much additional information

#################################################
# hierarchical clustering
#################################################
d <- dist(iris[, -5]) 
hc <- hclust(d, method = "complete")
plot(hc)
str(hc)

par(mfrow = c(2, 2))
hc <- hclust(d, method = "complete")
plot(hc, main = "complete link", xlab="", sub="")
hc <- hclust(d, method = "single")
plot(hc, main = "single link", xlab="", sub="")
hc <- hclust(d, method = "centroid")
plot(hc, main = "centroid link", xlab="", sub="")
hc <- hclust(d, method = "average")
plot(hc, main = "average link", xlab="", sub="")


plotdt <- data.frame(iris, cluster = cutree(hc, 3))
ggplot(plotdt, aes(Petal.Length, Petal.Width, color = as.factor(cluster))) +
    geom_point()
table(plotdt$cluster, plotdt$Species)

