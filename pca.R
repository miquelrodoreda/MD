setwd("/Users/miquelrodoreda/uni/MD")
df <- read.csv("dataset/renamed.csv")
directory = "pca/"
if (!dir.exists(directory)) {
  dir.create(directory, recursive = TRUE)
}

numericals <- which(sapply(df,is.numeric))
categoricals <- which(sapply(dd, function(col) is.character(col) || is.factor(col)))
df_numericals <- df[,numericals]
numericals_names <- names(df_numericals)
print(paste("The PCA analysis is going to be done for these numerical variables:", 
            paste(numericals_names, collapse=", ")))

principal_components <- prcomp(df_numericals, scale=TRUE)

print("PCA results:")
print(principal_components$rotation)

variance <- (principal_components$sdev)^2
variance_ratio <- variance / sum(variance)
cumulative_variance <- cumsum(variance_ratio)
png(paste0(directory, "variance.png"), width = 800, height = 600)
barplot(100 * variance_ratio, 
        main = "Variance Explained by Principal Components",
        xlab = "Principal Components", 
        ylab = "Variance Explained (%)", 
        col = "lightblue")
dev.off()
png(paste0(directory,"cumulative_variance.png"), width = 800, height = 600)
barplot(100 * cumulative_variance, 
        main = "Cumulative Variance Explained",
        xlab = "Principal Components", 
        ylab = "Cumulative Variance (%)", 
        col = "lightgreen")
dev.off()

# We will only compare dimensions 1 and 2 as they are the only significant ones (they almost reach the 80% of variance).
nd <- 2
Psi <- principal_components$x[,1:nd]
dim1 <- 1
dim2 <- 2
iden = row.names(df_numericals)

png(paste0(directory, "individuals_graphic.png"), width = 800, height = 600)
plot(Psi[, dim1], Psi[, dim2], main = "Individuals Graphic", xlab = "PC1", ylab = "PC2")
axis(side = 1, pos = 0, labels = FALSE, col = "cyan")
axis(side = 3, pos = 0, labels = FALSE, col = "cyan")
axis(side = 2, pos = 0, labels = FALSE, col = "cyan")
axis(side = 4, pos = 0, labels = FALSE, col = "cyan")
dev.off()

png(paste0(directory, "factorial_plane_only_numericals.png"), width = 800, height = 600)
ze = rep(0, length(numericals_names))
Phi = cor(df_numericals, Psi)
X <- Phi[, dim1]
Y <- Phi[, dim2]
plot(Psi[, dim1], Psi[, dim2], type = "n", xlim = c(min(X, 0), max(X, 0)), ylim = c(-1, 1))
axis(side = 1, pos = 0, labels = FALSE)
axis(side = 3, pos = 0, labels = FALSE)
axis(side = 2, pos = 0, labels = FALSE)
axis(side = 4, pos = 0, labels = FALSE)
arrows(ze, ze, X, Y, length = 0.07, col = "blue")
text(X, Y, labels = etiq, col = "darkblue", cex = 0.7)
dev.off()


colors<-rainbow(length(categoricals))
png(paste0(directory, "factorial_plane_numericals_with_categoricals.png"), width = 800, height = 600)
plot(Psi[, dim1], Psi[, dim2], type = "n", xlim = c(min(X, 0), max(X, 0)), ylim = c(-2, 1))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
arrows(ze, ze, X, Y, length = 0.07, col = "lightgray")
text(X, Y, labels = etiq, col = "gray", cex = 0.7)

c<-1
for(k in categoricals){
  nextColor<-colors[c]
  fdic1 = tapply(Psi[,dim1],df[,k],mean)
  fdic2 = tapply(Psi[,dim2],df[,k],mean) 
  
  text(fdic1,fdic2,labels=levels(factor(df[,k])),col=nextColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df)[categoricals],pch=1,col=colors, cex=0.6)
dev.off()


colors<-rainbow(length(categoricals))
png(paste0(directory, "factorial_plane_numericals_with_categoricals.png"), width = 800, height = 600)
plot(Psi[, dim1], Psi[, dim2], type = "n", xlim=c(-3.5,2), ylim=c(-2,1))
axis(side=1, pos= 0, labels = F, col="cyan")
axis(side=3, pos= 0, labels = F, col="cyan")
axis(side=2, pos= 0, labels = F, col="cyan")
axis(side=4, pos= 0, labels = F, col="cyan")
arrows(ze, ze, X, Y, length = 0.07, col = "lightgray")
text(X, Y, labels = etiq, col = "gray", cex = 0.7)

c<-1
for(k in categoricals){
  nextColor<-colors[c]
  fdic1 = tapply(Psi[,dim1],df[,k],mean)
  fdic2 = tapply(Psi[,dim2],df[,k],mean) 
  
  text(fdic1,fdic2,labels=levels(factor(df[,k])),col=nextColor, cex=0.6)
  c<-c+1
}
legend("bottomleft",names(df)[categoricals],pch=1,col=colors, cex=0.6)
dev.off()

# Grouping categoricals

generate_factorial_plane <- function(group_num, categories_names, xlim_vals, ylim_vals) {
  group_categories <- which(names(df) %in% categories_names)
  colors <- rainbow(length(group_categories))
  png(paste0(directory, "factorial_plane_group_", group_num, ".png"), width = 800, height = 600)
  plot(Psi[, dim1], Psi[, dim2], type = "n", xlim = xlim_vals, ylim = ylim_vals)
  axis(side = 1, pos = 0, labels = F, col = "cyan")
  axis(side = 3, pos = 0, labels = F, col = "cyan")
  axis(side = 2, pos = 0, labels = F, col = "cyan")
  axis(side = 4, pos = 0, labels = F, col = "cyan")
  arrows(ze, ze, X, Y, length = 0.07, col = "lightgray")
  text(X, Y, labels = etiq, col = "gray", cex = 0.7)
  c <- 1
  for (k in group_categories) {
    seguentColor <- colors[c]
    fdic1 <- tapply(Psi[, dim1], df[, k], mean)
    fdic2 <- tapply(Psi[, dim2], df[, k], mean)
    
    text(fdic1, fdic2, labels = levels(factor(df[, k])), col = seguentColor, cex = 0.6)
    c <- c + 1
  }
  legend("bottomleft", names(df)[group_categories], pch = 1, col = colors, cex = 0.6)
  dev.off()
}

# Group 1
categories_names <- c("price_level", "vegan_options", "gluten_free")
generate_factorial_plane(1, categories_names, c(-1,1.5), c(-2,1))

# Group 2
categories_names <- c("original_location", "cuisines")
generate_factorial_plane(2, categories_names, c(-1,1.5), c(-2,1))

# Group 3
categories_names <- c("meals", "awards")
generate_factorial_plane(3, categories_names, c(-3.5,1.5), c(-2,1))
