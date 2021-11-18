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

var.clust2 <- c("CODIBGE"  ,      "Ano"  ,          "DIV.EFETIVO"  , "DIV.PLANT.T" ,  "DIV.VL.T" ,     "DIV.VL.P", "DIV.VL.PRODANI")

#################################################################################################################
#
#   Diversity index data
#
#################################################################################################################
data <- read.csv(file="../data/entropy.diversity.indices.csv", header=TRUE, sep=",")

data.clust <- data[ var.clust]
data.clust <- replace(data.clust,is.na(data.clust),0)

data.clust2 <- data[ var.clust2 ]
data.clust2 <- replace(data.clust2,is.na(data.clust2),0)


# Kernel Density Plot
d <- density(data.clust$DIV.EFETIVO) # returns the density data
plot(d) # plots the results 

d <- density(log(data.clust$DIV.VL.P)) # returns the density data
plot(d) # plots the results 


# Kernel Density Plot
d <- density(data.clust$DIV.VL.P) # returns the density data
plot(d) # plots the results 

d <- density(log(data.clust$DIV.VL.P)) # returns the density data
plot(d) # plots the results 

data.clust$LOG_DIV.VL.P <-  log(data.clust$DIV.VL.P)

# Kernel Density Plot
d <- density(data.clust$DIV.VL.PRODANI) # returns the density data
plot(d) # plots the results 

d <- density(log(data.clust$DIV.VL.PRODANI)) # returns the density data
plot(d) # plots the results 

data.clust$LOG_DIV.VL.PRODANI <- log(data.clust$DIV.VL.PRODANI)



data.clust$T1_DIV.VL.P <- (data.clust$DIV.VL.P)^(1/3)
data.clust$T1_DIV.VL.PRODANI <- (data.clust$DIV.VL.PRODANI)^(1/3)
data.clust$SCL_DIV.EFETIVO <- scale(data.clust$DIV.EFETIVO)
data.clust$SCL_DIV.PLANT.T <- scale(data.clust$DIV.PLANT.T)
data.clust$SCL_DIV.VL.T <- scale(data.clust$DIV.VL.T)
data.clust$SCL_T1_DIV.VL.P <- scale(data.clust$T1_DIV.VL.P)
data.clust$SCL_T1_DIV.VL.PRODANI <- scale(data.clust$T1_DIV.VL.PRODANI)

library(mixAK)
mod <- GLMM_MCMC(y = data.clust[, c("SCL_DIV.EFETIVO"  , "SCL_DIV.PLANT.T" ,  "SCL_DIV.VL.T" ,  "SCL_T1_DIV.VL.P", "SCL_T1_DIV.VL.PRODANI")],
                 dist = c("gaussian", "gaussian", "gaussian", "gaussian", "gaussian"),
                 id = data.clust[, "CODIBGE"],
                 x = list(SCL_DIV.EFETIVO = "empty"  , SCL_DIV.PLANT.T = "empty" ,  SCL_DIV.VL.T = "empty" ,  SCL_T1_DIV.VL.P = "empty", SCL_T1_DIV.VL.PRODANI = "empty"),
                 z = list(SCL_DIV.EFETIVO = data.clust[, "Ano"]  , SCL_DIV.PLANT.T = data.clust[, "Ano"] ,  SCL_DIV.VL.T = data.clust[, "Ano"] ,  
                          SCL_T1_DIV.VL.P = data.clust[, "Ano"], SCL_T1_DIV.VL.PRODANI = data.clust[, "Ano"]),
                 random.intercept = rep(TRUE, 5),
                 prior.b = list(Kmax = 8),
                 nMCMC = c(burn = 100, keep = 10000, thin = 100, info = 1000),
                 parallel = FALSE)

mod <- NMixRelabel(mod, type = "stephens", keep.comp.prob = TRUE)

#Clustering procedure
groupMean <- apply(mod[[1]]$poster.comp.prob, 1, which.max)
pMean <- apply(mod[[1]]$poster.comp.prob, 1, max)
table(groupMean)

groupMed <- apply(mod[[1]]$quant.comp.prob[["50%"]], 1, which.max)
pMed <- apply(mod[[1]]$quant.comp.prob[["50%"]], 1, max)
table(groupMed)

table(groupMean, groupMed)

df.groupMed <- as.data.frame(groupMed)
df.groupMed$groupMed <- as.character(df.groupMed$groupMed)
df.groupMed <- within(df.groupMed, groupMed[df.groupMed == "2"] <- "A")
df.groupMed <- within(df.groupMed, groupMed[df.groupMed == "3"] <- "B")
df.groupMed <- within(df.groupMed, groupMed[df.groupMed == "4"] <- "C")
df.groupMed <- within(df.groupMed, groupMed[df.groupMed == "5"] <- "D")
df.groupMed <- within(df.groupMed, groupMed[df.groupMed == "6"] <- "E")
df.groupMed <- within(df.groupMed, groupMed[df.groupMed == "7"] <- "F")
table(df.groupMed)
df.groupMed$groupMed <- as.factor(df.groupMed$groupMed)

id = as.data.frame(unique(data.clust[, "CODIBGE"]))

df.groupMed <- cbind(df.groupMed, id)

colnames(df.groupMed) <- c("groupMed", "CODIBGE")

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
area.estudo@data <-  merge(area.estudo@data, df.groupMed, by.x = "CD_GEOCMU", by.y = "CODIBGE", all.x = TRUE )

writePolyShape( area.estudo, "../shapefile/BR_Shannon_Diversity_Clustering_ModelBased" )


data.clust <- cbind(data.clust, df.groupMed)
agg.C6.mean <- aggregate(data.clust[,3:7], by=list(data.clust$groupMed), FUN=mean)
agg.C6.sd <- aggregate(data.clust[,3:7], by=list(data.clust$groupMed), FUN=sd)