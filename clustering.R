# Cambiar el directorio de trabajo
setwd("/home/joan/Documents/Ricard/UPC/MD/MD")

# Cargar los datos
dd <- read.csv("dataset/renamed.csv")
names(dd)
dim(dd)
summary(dd)

attach(dd)

# Obtiene los nombres de las columnas numéricas
num_cols <- names(dd)[sapply(dd, is.numeric)]

# Filtra solo las columnas numéricas
dcon <- dd[, num_cols, drop = FALSE]  # drop=FALSE evita errores con una sola columna

# Verifica que dcon tiene datos
print(dim(dcon))

# Muestra las variables utilizadas en el clustering
print(paste("The clustering is going to be done for these numerical variables:", 
            paste(names(dcon), collapse=", ")))

#
# CLUSTERING
#

# KMEANS RUN, BUT HOW MANY CLASSES?
k1 <- kmeans(dcon, 5)
names(dcon)
print(k1)

attributes(k1)

k1$size
k1$withinss
k1$centers

# LET'S COMPUTE THE DECOMPOSITION OF INERTIA
Bss <- sum(rowSums(k1$centers^2) * k1$size)
Bss
Wss <- sum(k1$withinss)
Wss
Tss <- k1$totss
Tss

Bss + Wss

Ib1 <- 100 * Bss / (Bss + Wss)
Ib1

# LET'S REPEAT THE KMEANS RUN WITH K=5
k2 <- kmeans(dcon, 5)
k2$size

Bss <- sum(rowSums(k2$centers^2) * k2$size)
Bss
Wss <- sum(k2$withinss)
Wss

Ib2 <- 100 * Bss / (Bss + Wss)
Ib2
Ib1

k2$centers
k1$centers

# Guardar gráfico de los centros de los clusters
if (!dir.exists("clustering")) {
  dir.create("clustering")
}
png("clustering/kmeans_centers.png", width = 800, height = 600)
plot(k1$centers[, 3], k1$centers[, 2], main="KMeans: Centers of Clusters")
dev.off()

# Compara los clusters
table(k1$cluster, k2$cluster)

# Gráfico de distancias jerárquicas
png("clustering/hierarchical_clustering.png", width = 800, height = 600)
d <- dist(dcon)
h1 <- hclust(d, method="ward.D2")  # NOTICE THE COST
plot(h1, main="Hierarchical Clustering")
dev.off()

# Cortar el dendrograma y mostrar las clases
nc = 3
c1 <- cutree(h1, nc)

nc = 5
c5 <- cutree(h1, nc)

# Gráfico de partición por clases
png("clustering/clustering_partition.png", width = 800, height = 600)
plot(dcon$total_reviews_count, dcon$avg_rating, col=c1, main="Clustering: total_reviews_count vs avg_rating")
legend("topright", c("class1", "class2", "class3"), pch=1, col=c(1:3), cex=0.6)
dev.off()

# Boxplot de las variables
png("clustering/boxplot_avg_rating.png", width = 800, height = 600)
boxplot(dd[, 7] ~ c2, horizontal=TRUE, main="Boxplot: avg_rating by Clusters")
dev.off()

# Gráfico de clustering 2D
png("clustering/clustering_2D.png", width = 800, height = 600)
pairs(dcon[, 1:7], col=c1, main="Clustering: Pairwise Plot")
dev.off()

# Calidad de la partición jerárquica
Bss <- sum(rowSums(cdg^2) * as.numeric(table(c1)))

Ib4 <- 100 * Bss / Tss
Ib4

# Clustering con distancia Gower
library(cluster)

# Dissimilarity matrix
actives <- c(1:14)
dd[, sapply(dd, is.character)] <- lapply(dd[, sapply(dd, is.character)], as.factor)
str(dd)

dissimMatrix <- daisy(dd[, actives], metric = "gower", stand=TRUE)

distMatrix <- dissimMatrix^2

h1 <- hclust(distMatrix, method="ward.D")  # NOTICE THE COST
png("clustering/gower_clustering.png", width = 800, height = 600)
plot(h1, main="Gower Clustering")
dev.off()

c2 <- cutree(h1, 4)

# Visualización de los clusters
png("clustering/comparison_clusters.png", width = 800, height = 600)
plot(avg_rating, total_reviews_count, col=c2, main="Clustering of data in 3 classes")
legend("topright", legend = unique(c2), pch = 1, col = c(1:4), cex = 0.6)
dev.off()

# Gráfico de boxplot
png("clustering/boxplot_open_days.png", width = 800, height = 600)
boxplot(dd[, 6] ~ c2, horizontal=TRUE, main="Boxplot: open_days_per_week by Clusters")
dev.off()

# Visualiza las variables categóricas con boxplots
png("clustering/boxplot_total_reviews.png", width = 800, height = 600)
boxplot(dd[, 9] ~ c2, horizontal=TRUE, main="Boxplot: total_reviews_count by Clusters")
dev.off()

# Comparación de particiones
table(c1, c2)

# Visualización de los perfiles
cdg <- aggregate(as.data.frame(dcon), list(c2), mean)
png("clustering/clustering_profile.png", width = 800, height = 600)
plot(avg_rating, total_reviews_count, col=c2)
points(cdg[, 4], cdg[, 5], pch=16, col="orange")
text(cdg[, 4], cdg[, 5], labels=cdg[, 1], pos=2, font=2, cex=0.7, col="orange")
dev.off()

# Graficar relaciones entre variables
potencials <- c("open_days_per_week", "avg_rating", "food", "service")
png("clustering/pairs_plot.png", width = 800, height = 600)
pairs(dcon[, potencials], col=c2)
dev.off()
