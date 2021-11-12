################################################################
#
#   Working dir
#
################################################################
setwd("/media/marcos/DATA/projetos/GeoComputation21/manuscrito/PESQBASE/RScripts_desenv/R")

# Modelo de código para instalar pacotes caso necessário 
pkgs <- c("data.table", "sp", "rgeos", "rgdal", "dismo", "deldir", "maptools", "spdep", "tmap", "spacetime", "RColorBrewer", "ggplot2", "fields", "dplyr")

for (p in pkgs) {
  if (inherits(try(suppressMessages(library(p, character.only = TRUE))),
               "try-error"))
    install.packages(p, character.only = TRUE)
}


################################################################
#
#   Variables
#
################################################################



var.clust <- c("CODIBGE"  ,      "Ano"  ,          "DIV.EFETIVO"  , "DIV.PLANT.T" ,  "DIV.VL.T" ,     "DIV.VL.P", "DIV.VL.PRODANI",
               "DIV.AQU.VL" ,    "DIV.EXTV.VL"  ,  "DIV.SILV.VL")

#################################################################################################################
#
#   Diversity index data
#
#################################################################################################################
data <- read.csv(file="../data/entropy.diversity.indices.csv", header=TRUE, sep=",")

data.clust <- data[ var.clust]
data.clust <- replace(data.clust,is.na(data.clust),0)
#agg <- aggregate(data.clust[, 3:10], list(data.clust$Ano), mean)


library(reshape2)
m.data.clust <- acast(melt.data.clust, CODIBGE ~ Ano ~ variable)

ids <- unique(data.clust$CODIBGE)

library(kml3d)
data.3d <- cld3d(
  traj=m.data.clust,
  idAll=ids,
  time=1999:2018,
  varNames=c("DIV.EFETIVO"  , "DIV.PLANT.T" ,  "DIV.VL.T" ,     "DIV.VL.P", "DIV.VL.PRODANI",
    "DIV.AQU.VL" ,    "DIV.EXTV.VL"  ,  "DIV.SILV.VL" ))



kml3d(data.3d, nbClusters = 3:10, nbRedrawing = 20  )

numC3 <- getClusters(data.3d, 3)
numC4 <- getClusters(data.3d, 4)
numC5 <- getClusters(data.3d, 5)
numC6 <- getClusters(data.3d, 6)
numC7 <- getClusters(data.3d, 7)
numC8 <- getClusters(data.3d, 8)
numC9 <- getClusters(data.3d, 9)
numC10 <- getClusters(data.3d, 10)


BR.clusters <- data.frame(ids,numC3,numC4,numC5,numC6,numC7,numC8,numC9,numC10)

data.clust <- merge(data.clust, BR.clusters, by.x = "CODIBGE", by.y = "ids", all.x = TRUE )

write.csv(data.clust, file = "../data/entropy.diversity.indices_kmeans_clustering.csv", row.names = FALSE) 


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

writePolyShape( area.estudo, "../shapefile/BR_Shannon_Diversity_Clustering_kmeans" )

data.3d@c3[[20]]
data.3d@c4[[20]]
data.3d@c5[[20]]
data.3d@c6[[20]]
data.3d@c7[[20]]
data.3d@c8[[20]]
data.3d@c9[[20]]
data.3d@c10[[20]]


