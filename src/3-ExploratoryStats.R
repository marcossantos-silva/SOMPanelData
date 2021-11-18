################################################################
#
#   Working dir
#
################################################################
setwd("/media/marcos/DATA/projetos/GeoComputation21/manuscrito/PESQBASE/RScripts_desenv/R")

source("2-statsTable.R")

df.entropy.diversity.indices <-read.csv(file="../data/entropy.diversity.indices.csv", header=TRUE, sep=",") 

varlist <- c("DIV.EFETIVO","DIV.VL.PRODANI", "DIV.COLHI.T", "DIV.COLHI.P", "DIV.VL.T", "DIV.VL.P", "DIV.PLANT.T", "DIV.PLANT.P", "DIV.AQU.VL", "DIV.EXTV.VL", "DIV.SILV.VL")

statsTable( df.entropy.diversity.indices, varlist, "../data/basicStats.entrpy.indices.csv", decimal.format = "." ) 
