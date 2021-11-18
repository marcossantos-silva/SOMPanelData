# Modelo de código para instalar pacotes caso necessário 
pkgs <- c("data.table", "sp", "rgeos", "rgdal", "dismo", "deldir", "maptools", "spdep", "tmap", "spacetime", "RColorBrewer", "ggplot2", "e1071")

for (p in pkgs) {
  if (inherits(try(suppressMessages(library(p, character.only = TRUE))),
               "try-error"))
    install.packages(p, character.only = TRUE)
}

# Create the function.
getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}


statsTable <- function( data, varlista, filename, decimal.format = "." ) {
  
  
  sink(filename)
  cat(paste("VarName",";",
    "Variance", ";",
    "Standard Deviation",";",
    "Mean",";",
    "Median",";",
    "Min",";",
    "Max",";",
    "Skewness",";",
    "Kurtosis",";",
    "Mode",";",
    "1st quantile",";",
    "3rd quantile", ";",
    "NA", sep=""
  )
  , file=filename, sep="\n")
  for (i in 1:length(varlista)) {

    cat(paste( varlista[i],";",
      format(var(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark= decimal.format), ";",
      format(sd(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(mean(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(median(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(min(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(max(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(skewness(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(kurtosis(data[,varlista[i]],  na.rm = TRUE),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(getmode(data[,varlista[i]]),digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(quantile(data[,varlista[i]],  na.rm = TRUE)[2],digits=2,nsmall=2, decimal.mark=decimal.format),";",
      format(quantile(data[,varlista[i]],  na.rm = TRUE)[4],digits=2,nsmall=2, decimal.mark=decimal.format), ";",
      format(sum(is.na(data[,varlista[i]])),  nsmall=0, decimal.mark=decimal.format),"\n"
      ,  sep="")
    , file=filename, append=TRUE)
       
  }
  
  sink()
  return(0)
}

