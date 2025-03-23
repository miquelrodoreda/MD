
# Cargamos los datos
path <- "E:/PROYECTOS/carlesL/"
fichero <- "renamed.RData"
renamed <- get(load(paste0(path, fichero)))

# Preprocessing
### SR: No hacemos debido a que ya está preprocesado
## Detectamos las clases de las variables
tipo <- sapply(renamed, class)
varCat <- names(tipo)[which(tipo %in% c("character", "factor"))]

## Convertimos a factor las variables "character"
for(vC in varCat){renamed[, vC] <- as.factor(renamed[, vC])}

# Clustering 
## Calculamos la distancias
distancias <- cluster::daisy(renamed, metric = "gower"); distancias <- distancias^2

## Aplicamos la agregación del dendograma
clusteres <- stats::hclust(distancias, method = "ward.D2")
plot(clusteres)

### SR: Visualizamos que el mejor corte teorico son 2 clases pero que, por interpretación
### del caso de estudio generaremos 3 clases
library(dendextend)
clusteres %>% as.dendrogram %>% set("branches_k_color", k = 3) %>% plot()

renamed$cortes <- as.character(stats::cutree(clusteres, k = 3))

# Profiling
## Vamos a utilizar la metodologia de los scripts de profiling para poder obtener como son nuestras
## 3 clases

### Cargamos las funciones necesarias para hacer el profiling
source(paste0(path, "funcionesProfiling.R"))

### Aplicamos el estudio para cada variable 
dades <- renamed                # Base de datos
K <- ncol(dades)                # Número de variables
nameP <- "cortes"               # Nombre de la particion
P <- dades[, nameP]             # Particion
nc <- length(levels(factor(P))) # Número de particiones disponibles
n <- nrow(dades)                # Número de individuos 

pvalk <- matrix(data = 0, nrow = nc, ncol = K, 
                dimnames = list(levels(P), names(dades)))


### Vamos aplicar el estudio para cada variable. Vamos a modificar un poco la función
### para que nos guarde los gráficos y los outputs en diferentes documentos

#### STEP1: Creamos la carpeta en caso de no existir
pathProfiling <- paste0(path, "Profiling/")
if (!dir.exists(pathProfiling)) {dir.create(pathProfiling, recursive = T)}

#### STEP2: Creamos el perfilado de cada una de las variables
for(k in 1:K){
  sink(paste0(pathProfiling, "Profiling_", colnames(dades)[k], ".txt"))
  # VARIABLES NUMERICAS:  
  if (is.numeric(dades[, k])){ 
    # Iniciamos la variable
    print(paste("Anàlisi per classes de la Variable:", names(dades)[k]))
    
    # Generamos el boxplot
    png(filename = paste0(pathProfiling, "boxplot_", colnames(dades)[k], ".png"))
    boxplot(dades[,k] ~ P, main = paste("Boxplot of", names(dades)[k], "vs", nameP), 
            horizontal = TRUE, xlab = colnames(dades)[k])
    dev.off()
    
    # Generamos el barplot de medias
    png(filename = paste0(pathProfiling, "barplot_medias_", colnames(dades)[k], ".png"))
    barplot(tapply(dades[[k]], P, mean), main = paste("Means of", names(dades)[k], "by", nameP))
    abline(h = mean(dades[[k]]), col = "red")
    legend(0, mean(dades[[k]]), "global mean", bty = "n")
    dev.off()
    
    
    print("Estadístics per groups:")
    for(s in levels(as.factor(P))) {
      print(summary(dades[P == s, k]))
    }
    
    o <- oneway.test(dades[, k] ~ P)
    print(paste("p-valueANOVA:", o$p.value))
    
    kw <- kruskal.test(dades[, k] ~ P)
    print(paste("p-value Kruskal-Wallis:", kw$p.value))
    
    pvalk[, k] <- ValorTestXnum(dades[, k], P)
    
    print("p-values ValorsTest: ")
    print(pvalk[, k])      
  } 
  else if(class(dades[, k]) == "Date"){
      print(summary(dades[, k]))
      print(sd(dades[, k]))
      # decide breaks: weeks, months, quarters...
      hist(dades[, k], breaks = "weeks")
    } 
  else {
      # qualitatives
      print(paste("Variable", names(dades)[k]))
      table <- table(P, dades[, k])
      rowperc <- prop.table(table, 1)
      colperc <- prop.table(table, 2)

      # ojo porque si la variable es true o false la identifica amb el tipus Logical i
      # aquest no te levels, por tanto, coertion preventiva
    
      dades[, k] <- as.factor(dades[, k])

      marg <- table(as.factor(P))/n
      print(append("Categories = ",levels(as.factor(dades[, k]))))
      
      # from next plots, select one of them according to your practical case
      png(filename = paste0(pathProfiling, "snake_", colnames(dades)[k], "_notLegend.png"))
      plot(marg, type = "l", ylim = c(0, 1), 
           main = paste("Prop. of pos & neg by", names(dades)[k]))
      paleta <- rainbow(length(levels(dades[, k])))
      for(c in 1:length(levels(dades[, k]))){lines(colperc[, c], col = paleta[c])}
      dev.off()
      
      # with legend
      png(filename = paste0(pathProfiling, "snake_", colnames(dades)[k], "_Legend.png"))
      plot(marg, type = "l", ylim = c(0, 1), main = paste("Prop. of pos & neg by", names(dades)[k]))
      paleta <- rainbow(length(levels(dades[, k])))
      for(c in 1:length(levels(dades[, k]))){lines(colperc[, c], col = paleta[c])}
      legend("topright", levels(dades[, k]), col = paleta, lty = 2, cex = 0.6)
      dev.off()
      
      # condicionades a classes
      print("Condicionadas a clases:\n")
      print(append("Categories = ",levels(dades[, k])))
      
      png(filename = paste0(pathProfiling, "snake_", colnames(dades)[k], "_cond_notLegend.png"))
      plot(marg, type = "n", ylim = c(0,1), main = paste("Prop. of pos & neg by", names(dades)[k]))
      paleta <- rainbow(length(levels(dades[, k])))
      for(c in 1:length(levels(dades[, k]))){lines(rowperc[, c], col = paleta[c]) }
      dev.off()
      
      # with legend
      png(filename = paste0(pathProfiling, "snake_", colnames(dades)[k], "_cond_Legend.png"))
      plot(marg, type = "n", ylim = c(0, 1), main = paste("Prop. of pos & neg by", names(dades)[k]))
      paleta <- rainbow(length(levels(dades[, k])))
      for(c in 1:length(levels(dades[, k]))){lines(rowperc[, c],col = paleta[c])}
      legend("topright", levels(dades[, k]), col= paleta, lty = 2, cex = 0.6)
      dev.off()
      
      # amb variable en eix d'abcisses
      marg <- table(dades[, k])/n
      print(append("Categories = ",levels(dades[, k])))
      
      png(filename = paste0(pathProfiling, "snake_mod_", colnames(dades)[k], "_notLegend.png"))
      plot(marg,type = "l", ylim = c(0, 1), 
           main = paste("Prop. of pos & neg by", names(dades)[k]), las = 3)
      paleta <- rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){
        lines(rowperc[c, ], col = paleta[c])
      }
      dev.off()
      
      # with legend
      png(filename = paste0(pathProfiling, "snake_mod_", colnames(dades)[k], "_Legend.png"))
      plot(marg, type = "l", ylim = c(0, 1), 
           main = paste("Prop. of pos & neg by", names(dades)[k]), las = 3)
      for(c in 1:length(levels(as.factor(P)))){
        lines(rowperc[c, ], col = paleta[c])
        }
      legend("topright", levels(as.factor(P)), col = paleta, lty = 2, cex = 0.6)
      dev.off()
      
      # condicionades a columna 
      png(filename = paste0(pathProfiling, "snake_mod_", colnames(dades)[k], "cond_notLegend.png"))
      plot(marg, type = "n", ylim = c(0, 1), 
           main = paste("Prop. of pos & neg by",names(dades)[k]), las = 3)
      paleta <- rainbow(length(levels(as.factor(P))))
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c, ],col = paleta[c])}
      dev.off()
      
      # with legend
      png(filename = paste0(pathProfiling, "snake_mod_", colnames(dades)[k], "cond_Legend.png"))
      plot(marg, type = "n", ylim = c(0, 1), main = paste("Prop. of pos & neg by",names(dades)[k]), las = 3)
      for(c in 1:length(levels(as.factor(P)))){lines(colperc[c, ], col = paleta[c])}
      legend("topright", levels(as.factor(P)), col = paleta, lty = 2, cex = 0.6)
      dev.off()
      
      table <- table(dades[, k], P)
      print("Cross Table:")
      print(table)
      print("Distribucions condicionades a columnes:")
      print(colperc)
      
      # diagrames de barres apilades                                         
      png(filename = paste0(pathProfiling, "barplotApil_", colnames(dades)[k], "_notLegend.png"))
      paleta <- rainbow(length(levels(dades[, k])))
      barplot(table(dades[, k], as.factor(P)), beside = FALSE,col = paleta)
      dev.off()
      
      png(filename = paste0(pathProfiling, "barplotApil_", colnames(dades)[k], "_Legend.png"))
      barplot(table(dades[, k], as.factor(P)), beside = FALSE, col = paleta)
      legend("topright", levels(as.factor(dades[, k])), pch = 1, cex = 0.5, col = paleta)
      dev.off()
      
      # diagrames de barres adosades
      png(filename = paste0(pathProfiling, "barplotAdos_", colnames(dades)[k], "_notLegend.png"))
      barplot(table(dades[, k], as.factor(P)), beside = TRUE, col = paleta)
      dev.off()

      png(filename = paste0(pathProfiling, "barplotAdos_", colnames(dades)[k], "_Legend.png"))
      barplot(table(dades[, k], as.factor(P)), beside = TRUE,col = paleta)
      legend("topright",levels(as.factor(dades[, k])), pch = 1, cex = 0.5, col = paleta)
      dev.off()
      
      print("Test Chi quadrat: ")
      print(chisq.test(dades[, k], as.factor(P)))
      
      print("valorsTest:")
      print( ValorTestXquali(P, dades[, k]))
      # calcular els pvalues de les quali
  }
  sink()
}


# descriptors de les classes més significatius. Afegir info qualits
sink(paste0(pathProfiling, "PvaluesXClusters.txt"))
for (c in 1:length(levels(as.factor(P)))) {
  if(!is.na(levels(as.factor(P))[c])){
    print(paste("P.values per class:", levels(as.factor(P))[c]));
    print(sort(pvalk[c, ]), digits = 3) 
  }
}
sink()

# ==============================================================================
# Otra forma de hacerlo también nos permite identificar patrones 
library(FactoMineR)

png(filename = paste0(pathProfiling, "profiling_barplot_quali.png"))
plot(FactoMineR::catdes(donne = dades, num.var = ncol(dades)), show = "quali", cex = 1.2)
dev.off()

png(filename = paste0(pathProfiling, "profiling_tabular_quanti.png"))
plot(FactoMineR::catdes(donne = dades, num.var = ncol(dades)), show = "quanti", cex = 1.2)
dev.off()

par(mar = c(8, 4, 4, 2) + 0.1)
png(filename = paste0(pathProfiling, "profiling_barplot_quanti.png"))
plot(FactoMineR::catdes(donne = dades, num.var = ncol(dades)), show = "quanti", cex = 1.2, 
     barplot = T)
dev.off()
par(mfrow = c(1, 1), mar = c(5, 4, 4, 2) + 0.1))
