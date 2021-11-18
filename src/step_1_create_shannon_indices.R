# Modelo de código para instalar pacotes caso necessário 
pkgs <- c("data.table", "sp", "rgeos", "rgdal", "dismo", "deldir", "maptools", "spdep", "tmap", "spacetime", "RColorBrewer", "ggplot2", "fields", "dplyr")

for (p in pkgs) {
  if (inherits(try(suppressMessages(library(p, character.only = TRUE))),
               "try-error"))
    install.packages(p, character.only = TRUE)
}

################################################################
#
#   Working dir
#
################################################################
setwd("/media/marcos/DATA/projetos/GeoComputation21/manuscrito/PESQBASE/RScripts/R")


#Shannon entropy index
entropy.index <- function( cat ) {
  result <- 0
  for (i in 1:length(cat)) {
    if (cat[i] > 0) {
      result <- result + (cat[i]/sum(cat))*log(cat[i]/sum(cat), length(cat))
    }
  }
  return(-1*result)
}


df.raw.panel.data <-read.csv(file="../data/rawdata/longitudinal.data.v.github_1.csv", header=TRUE, sep=",") 


DIV.EFETIVO <- 10:20
df.raw.panel.data[DIV.EFETIVO ] <- sapply(df.raw.panel.data[DIV.EFETIVO], as.numeric)
df.raw.panel.data[DIV.EFETIVO ] <- replace(df.raw.panel.data[DIV.EFETIVO ],is.na(df.raw.panel.data[DIV.EFETIVO ]),0)
df.raw.panel.data$DIV.EFETIVO <- apply(df.raw.panel.data[DIV.EFETIVO], 1, entropy.index)
summary(df.raw.panel.data$DIV.EFETIVO)
hist(df.raw.panel.data$DIV.EFETIVO)


DIV.VL.PRODANI <- 21:26
df.raw.panel.data[DIV.VL.PRODANI ] <- sapply(df.raw.panel.data[DIV.VL.PRODANI], as.numeric)
df.raw.panel.data[DIV.VL.PRODANI ] <- replace(df.raw.panel.data[DIV.VL.PRODANI],is.na(df.raw.panel.data[DIV.VL.PRODANI ]),0)
df.raw.panel.data$DIV.VL.PRODANI <- apply(df.raw.panel.data[DIV.VL.PRODANI], 1, entropy.index)
summary(df.raw.panel.data$DIV.VL.PRODANI)
hist(df.raw.panel.data$DIV.VL.PRODANI)


DIV.COLHI.T <- 29:61
df.raw.panel.data[DIV.COLHI.T ] <- sapply(df.raw.panel.data[DIV.COLHI.T], as.numeric)
df.raw.panel.data[DIV.COLHI.T ] <- replace(df.raw.panel.data[DIV.COLHI.T],is.na(df.raw.panel.data[DIV.COLHI.T]),0)
df.raw.panel.data$DIV.COLHI.T <- apply(df.raw.panel.data[DIV.COLHI.T], 1, entropy.index)
summary(df.raw.panel.data$DIV.COLHI.T)
hist(df.raw.panel.data$DIV.COLHI.T)


DIV.COLHI.P <- 62:99
df.raw.panel.data[DIV.COLHI.P ] <- sapply(df.raw.panel.data[DIV.COLHI.P], as.numeric)
df.raw.panel.data[DIV.COLHI.P ] <- replace(df.raw.panel.data[DIV.COLHI.P],is.na(df.raw.panel.data[DIV.COLHI.P]),0)
df.raw.panel.data$DIV.COLHI.P <- apply(df.raw.panel.data[DIV.COLHI.P], 1, entropy.index)
summary(df.raw.panel.data$DIV.COLHI.P)
hist(df.raw.panel.data$DIV.COLHI.P)


DIV.VL.T  <- 327:369
df.raw.panel.data[DIV.VL.T ] <- sapply(df.raw.panel.data[DIV.VL.T], as.numeric)
df.raw.panel.data[DIV.VL.T ] <- replace(df.raw.panel.data[DIV.VL.T],is.na(df.raw.panel.data[DIV.VL.T]),0)
df.raw.panel.data$DIV.VL.T <- apply(df.raw.panel.data[DIV.VL.T], 1, entropy.index)
summary(df.raw.panel.data$DIV.VL.T)
hist(df.raw.panel.data$DIV.VL.T)


DIV.VL.P  <- 370:407
df.raw.panel.data[DIV.VL.P ] <- sapply(df.raw.panel.data[DIV.VL.P], as.numeric)
df.raw.panel.data[DIV.VL.P ] <- replace(df.raw.panel.data[DIV.VL.P],is.na(df.raw.panel.data[DIV.VL.P]),0)
df.raw.panel.data$DIV.VL.P <- apply(df.raw.panel.data[DIV.VL.P], 1, entropy.index)
summary(df.raw.panel.data$DIV.VL.P)
hist(df.raw.panel.data$DIV.VL.P)


DIV.PLANT.T  <- 408:439
DIV.PLANT.T <- c(DIV.PLANT.T, 441)
df.raw.panel.data[DIV.PLANT.T ] <- sapply(df.raw.panel.data[DIV.PLANT.T], as.numeric)
df.raw.panel.data[DIV.PLANT.T ] <- replace(df.raw.panel.data[DIV.PLANT.T],is.na(df.raw.panel.data[DIV.PLANT.T]),0)
df.raw.panel.data$DIV.PLANT.T <- apply(df.raw.panel.data[DIV.PLANT.T], 1, entropy.index)
summary(df.raw.panel.data$DIV.PLANT.T)
hist(df.raw.panel.data$DIV.PLANT.T)


DIV.PLANT.P  <- 442:479
df.raw.panel.data[DIV.PLANT.P ] <- sapply(df.raw.panel.data[DIV.PLANT.P], as.numeric)
df.raw.panel.data[DIV.PLANT.P ] <- replace(df.raw.panel.data[DIV.PLANT.P],is.na(df.raw.panel.data[DIV.PLANT.P]),0)
df.raw.panel.data$DIV.PLANT.P <- apply(df.raw.panel.data[DIV.PLANT.P], 1, entropy.index)
summary(df.raw.panel.data$DIV.PLANT.P)
hist(df.raw.panel.data$DIV.PLANT.P)


DIV.AQU.VL <- 283:306
df.raw.panel.data[DIV.AQU.VL ] <- sapply(df.raw.panel.data[DIV.AQU.VL], as.numeric)
df.raw.panel.data[DIV.AQU.VL ] <- replace(df.raw.panel.data[DIV.AQU.VL],is.na(df.raw.panel.data[DIV.AQU.VL]),0)
df.raw.panel.data$DIV.AQU.VL <- apply(df.raw.panel.data[DIV.AQU.VL], 1, entropy.index)
summary(df.raw.panel.data$DIV.AQU.VL)
hist(df.raw.panel.data$DIV.AQU.VL)


DIV.EXTV.VL <- 215:258
df.raw.panel.data[DIV.EXTV.VL ] <- sapply(df.raw.panel.data[DIV.EXTV.VL], as.numeric)
df.raw.panel.data[DIV.EXTV.VL ] <- replace(df.raw.panel.data[DIV.EXTV.VL],is.na(df.raw.panel.data[DIV.EXTV.VL]),0)
df.raw.panel.data$DIV.EXTV.VL <- apply(df.raw.panel.data[DIV.EXTV.VL], 1, entropy.index)
summary(df.raw.panel.data$DIV.EXTV.VL)
hist(df.raw.panel.data$DIV.EXTV.VL)



DIV.SILV.VL <- 322:336
df.raw.panel.data[DIV.SILV.VL ] <- sapply(df.raw.panel.data[DIV.SILV.VL], as.numeric)
df.raw.panel.data[DIV.SILV.VL ] <- replace(df.raw.panel.data[DIV.SILV.VL],is.na(df.raw.panel.data[DIV.SILV.VL]),0)
df.raw.panel.data$DIV.SILV.VL <- apply(df.raw.panel.data[DIV.SILV.VL], 1, entropy.index)
summary(df.raw.panel.data$DIV.SILV.VL)
hist(df.raw.panel.data$DIV.SILV.VL)

write.csv(df.raw.panel.data, file = "../data/indices/entropy.diversity.indices.csv", row.names = FALSE) 
