################################################################
#
#   Working dir
#
################################################################
setwd("/media/marcos/DATA/projetos/GeoComputation21/manuscrito/PESQBASE/RScripts_desenv/R")

################################################################
#
#   Lista de pacotes R a serem carregados
#
################################################################
pkgs <- c("kohonen", "eHOF", "rgdal", "maptools"
)

pretty_palette <- c("#1f77b4", '#ff7f0e', '#2ca02c', '#d62728', '#9467bd', '#8c564b', '#e377c2',"#F08080") 

#Carrega pacotes necessários e os instala caso necessário
for (p in pkgs) {
  if (inherits(try(suppressMessages(library(p, character.only = TRUE))),
               "try-error"))
    install.packages(p, character.only = TRUE)
}

data <- read.csv(file="../data/entropy.diversity.indices.csv", header=TRUE, sep=",") 
data <- data[c("CODIBGE", "Ano", "DIV.EFETIVO", "DIV.PLANT.T", "DIV.VL.T", "DIV.VL.P", "DIV.VL.PRODANI", "DIV.AQU.VL", "DIV.EXTV.VL", "DIV.SILV.VL")]
var.clust.traj <- 3:10

#################################################
################################################################
#
#   Configurando a estrutura da RNA SOM
#
################################################################
set.seed(7)

som_grid <- somgrid(xdim = 25, ydim= 30, topo="hexagonal")

################################################################
#
#   Chamando a função de Aprendizado de Máquina da RNA SOM
#
################################################################
som_model <- som(as.matrix(data[,var.clust.traj]), 
                 grid=som_grid, 
                 rlen=5, 
                 alpha=c(0.05,0.01), 
                 keep.data = TRUE
)
#################################################
#
# Escolha da RNA SOM Treinada que será usada
#
#################################################

somTrained <- read.csv(file="../SOMPAK/map25x30trainedv1_AJUSTADO.cod", header=FALSE, sep=" ") 
somTrained <- somTrained[,c(1:8)]

som_model$codes[[1]] <- as.matrix(somTrained)


classif <- map(som_model, as.matrix(data[,var.clust.traj]), maxNA.fraction = 1)


som_model$unit.classif <- classif$unit.classif
som_model$distances <-classif$distances
som_model$whatmap <- classif$whatmap
som_model$user.weights <- classif$user.weights

plot(som_model, type="count", main="Number of observations associated to each neuron")

#Mostra todas as observações na grade neural
plot(som_model, type = "mapping",   pch = 1, main = "All observations", keepMargins = TRUE)

################################################################
#
#   Avaliação do método k-means pelo método do cotovelo
#
################################################################
mydata <- som_model$codes[[1]] 
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var)) 
for (i in 2:15) {
  wss[i] <- sum(kmeans(mydata, iter.max = 30, centers=i)$withinss)
}
dev.off()
plot(wss, 
     main = "Elbow curve for K-means \ncodevectors clustering",
     type = "b", pch = 19, frame = FALSE, 
     xlab = "k",
     ylab = "Within squared sum error",
     cex=1.5,
     cex.lab=1.5,
     cex.axis=1.0,
     cex.main =1.5,
     xlim = c(0,15))

################################################################
#
#   Avaliação do k-means pelo método Silhouette
#
################################################################
library(tidyverse)
library(cluster)
mydata <- som_model$codes[[1]] 
# # Clacula a média Silhouette para k clusters
avg_sil <- function(k) {
  km.res <- kmeans(mydata, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(mydata))
  mean(ss[, 3])
}
# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15
# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)
dev.off()
plot(k.values, avg_sil_values,
     main = "Evaluating k \nSilhouette method for k-means",
     type = "b", pch = 19, frame = FALSE, 
     xlab = "k",
     ylab = "Silhouette cluster quality index (mean)",
     cex=1.5,
     cex.lab=1.5,
     cex.axis=1.0,
     cex.main =1.5,
     xlim = c(0,15))

som_cluster <- kmeans(mydata, centers = 6, nstart = 25, iter.max = 25)
label.data <- rep("", dim(data)[1])
plot(som_model, type="mapping", 
     bgcol = pretty_palette[som_cluster$cluster], 
     labels = label.data, 
     main = "", 
     cex=1.5,
     cex.lab=1.5,
     cex.axis=1.0 )
title("25x30 hexagonal SOM clustered using\nK-means method (k=6)", line = +2.2, cex=2.5)
add.cluster.boundaries(som_model, som_cluster$cluster)
legend(2,20,  inset=.02, title="Clusters",
       c("1","2","3","4","5","6"), fill=pretty_palette, horiz=FALSE, cex=1.1)

  
  
  