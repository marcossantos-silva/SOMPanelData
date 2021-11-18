################################################################
#
#   Working dir
#
################################################################
setwd("/media/marcos/DATA/projetos/GeoComputation21/manuscrito/PESQBASE/RScripts_desenv/R")

###############################################################
#
# source("6-ComponentPlanes.R")
#
##############################################################

################################################################
#
#   Working dir
#
################################################################
setwd("/media/marcos/DATA/projetos/GeoComputation21/manuscrito/PESQBASE/RScripts_desenv/R")

###############################################################
#
#source("5-ClusterizacaoSOM.R")
#
##############################################################
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





################################################################
#
#  Component Planes
#
################################################################
#png(filename = "Figure4.png", width = 14, height = 14, units = "in", res=600)
coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
#par(mfrow = c(3, 3))
layout(autolayout(8))
for (i in 1:length(var.clust.traj)) {
  plot(som_model, type = "property", 
       property = getCodes(som_model)[,i], 
       main="", palette.name=coolBlueHotRed, cex=1.25, cex.axis=0.8) 
  title(colnames(data[,var.clust.traj])[i], line = +.5, cex.main=2)
  add.cluster.boundaries(som_model, som_cluster$cluster, lwd = 4)
}
#dev.off()

################################################################
#
#  Associa cada data de entrada ao cluster da RNA SOM k=6
#  
#
################################################################
cluster_assignment <- som_cluster$cluster[som_model$unit.classif]
table(cluster_assignment)
data$clusterSOMk6 <- cluster_assignment





########################################################################
#
# Análise de trajetória
#
########################################################################

coords <- as.data.frame(som_model$grid$pts)


#Gerando, para cada observação-ano, duas colunas (x e y) das coordenadas do seu BMU
data$x <- NA
data$y <- NA
for (i in 1:length(som_model$unit.classif)) {
  data[i,"x"]  <-  coords[som_model$unit.classif[i],"x"]
  data[i,"y"]  <-  coords[som_model$unit.classif[i],"y"]
}

#Clusterização das trajetórias usando método de distãncia de matrizes

#Primeiramente formata o dado de acordo com a necessidade do pacote kml
library(reshape2)
data.traj <- data[c("CODIBGE","Ano","x","y")]
melt.data.clust <- melt(data.traj, id.vars = c("CODIBGE","Ano"))
m.data.clust <- acast(melt.data.clust, CODIBGE ~ Ano ~ variable)
ids <- unique(data.traj$CODIBGE)
library(kml3d)
data.3d <- cld3d(
  traj=m.data.clust,
  idAll=ids,
  time=1999:2018,
  varNames=c("x", "y" ))

#Efeutua a clusterização, testando k=3..10
kml3d(data.3d, nbClusters = 3:10, nbRedrawing = 20  )

#Recupera os agrupamentos
numC3 <- getClusters(data.3d, 3)
numC4 <- getClusters(data.3d, 4)
numC5 <- getClusters(data.3d, 5)
numC6 <- getClusters(data.3d, 6)
numC7 <- getClusters(data.3d, 7)
numC8 <- getClusters(data.3d, 8)
numC9 <- getClusters(data.3d, 9)
numC10 <- getClusters(data.3d, 10)

#Transforma em data frame
BR.clusters <- data.frame(ids,numC3,numC4,numC5,numC6,numC7,numC8,numC9,numC10)

data <- merge(data, BR.clusters, by.x = "CODIBGE", by.y = "ids", all.x = TRUE )

write.table(x=data, file="../data/entropy.diversity.indices.RESULTADOS.csv", quote=FALSE, sep = ";", row.names = FALSE)

#agg.C8.mean <- aggregate(data[,3:10], by=list(data$numC8), FUN=mean)
#agg.C8.sd <- aggregate(data[,3:10], by=list(data$numC8), FUN=sd)

################################################################
#
#   Projetando as médias dos clusters das trajetórias no mapa neural
#   
#
################################################################


  colors <- c("#a0e85b", "#8d1993", "#63b1f3", "#4c3e76", "#1abdc5", "#154e56", "#fcc2fb", "#2d7a2c")
  grupos <- c("A","B","C","D","E","F","G","H")
  
  distTrajlist <- list()
  distTraj <- data[which(data$Ano %in% c(1999,2018)),]
  for (i in 1:length(ids)) {
    obs.tmp <- distTraj[which(distTraj$CODIBGE==ids[i]), c("Ano","x","y")]
    dist.tmp <- (obs.tmp$x[1] - obs.tmp$x[2])*(obs.tmp$x[1] - obs.tmp$x[2]) + (obs.tmp$y[1]-obs.tmp$y[2])*(obs.tmp$y[1]-obs.tmp$y[2])
    distTrajlist[[i]] <- c(ids[i], dist.tmp)
  }
  df.distTraj <- data.frame(matrix(unlist(distTrajlist), nrow=length(distTrajlist), byrow=TRUE),stringsAsFactors=FALSE)
  colnames(df.distTraj) <- c("ids","dist")
  df.distTrajC8 <- merge(df.distTraj, BR.clusters[c("ids","numC8")], by.x = "ids", by.y = "ids", all.x = TRUE )
  
    aggTrajc8 <- aggregate(data[, c("x","y")], list(data$Ano,data$numC8), mean)
  initTrajc8 <- aggTrajc8[which(aggTrajc8$Group.1==1999),c("x","y")]
  endTrajc8 <-  aggTrajc8[which(aggTrajc8$Group.1==2018),c("x","y")]
  
  
  png(filename = "../images/FigureMeanTrajectories.png", width = 7, height = 8, units = "in", res=600)
  label.data <- rep("", dim(data)[1])
  plot(som_model, type="mapping", bgcol = NULL, labels = label.data, 
       main = "Mean trajectories", 
       cex.lab=1.5,
       cex.axis=1.5,
       cex.main =1.5)
  for (i in 1:8) {
    lines( aggTrajc8[which(as.character(aggTrajc8$Group.2)==grupos[i]),c("x","y")], col=colors[i], lwd=4)
  }
  add.cluster.boundaries(som_model, som_cluster$cluster, lwd = 4)  
  points(initTrajc8, pch = 16 )
  points(endTrajc8, pch = 17 )
  legend(8.4,4.1,  inset=.02, title="Trajectory on neural map cluster",
         grupos, fill=colors, horiz=TRUE, cex=.8)
  legend( 10,7, 
          legend=c("Begin of the trajectory","End of the trajectory"), 
          col=c("black"), 
          pch=c(16,17), merge=FALSE, horiz=FALSE, cex=.8 )
  dev.off()
  
################################################################
#
#   Salvando os dados na base geográfica
#   
#
################################################################
library(rgdal)
#Lendo o shapefile da area de estudo shapefile
area.estudo <- readOGR("../shapefile/", "BR_Municipios_2020" )

#Merge
area.estudo@data <- merge(area.estudo@data, BR.clusters, by.x = "CD_MUN", by.y = "ids", all.x = TRUE )


################################################################
#
#   Definição manual das três regiões a partir da observação
#   da distribuição espacial dos oito grupos numC8
#
################################################################

area.estudo@data$REGIAOC8 <- NA
regiaog8.grp1 <- c("MA","CE","RN","PB","PE","AL","SE","BA","PI","MG","DF","TO","GO")
regiaog8.grp2 <- c("MS","MT","AM","RO","RR","PA","AP","AC")

area.estudo@data[which(area.estudo@data$SIGLA_UF %in% regiaog8.grp1),"REGIAOC8"] <- "1"
area.estudo@data[which(area.estudo@data$SIGLA_UF %in% regiaog8.grp2),"REGIAOC8"] <- "2"
area.estudo@data[which(is.na(area.estudo@data$REGIAOC8)),"REGIAOC8"] <- "3"

#Frequencia de cada cluster por região
table(area.estudo@data$numC8)
df.sp.data <- as.data.frame(table(area.estudo@data$REGIAOC8,area.estudo@data$numC8))
write.table(x=df.sp.data, file="../data/FREQ_REGIAOC8_GRUPOSTRAJC8.csv", quote=FALSE, sep = ";", row.names = FALSE)

#Salvando resultados no BDG
library(maptools)
writePolyShape( area.estudo, "../shapefile/BR_TrajectoryClusters_3Regions" )


  
  
  