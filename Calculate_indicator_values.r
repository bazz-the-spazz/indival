

# Function to calculate weighted or mean indicator values
## data has to be in the form: columns are species, rows are plots, rownames are the names of the plots


get.indicator.value <- function(d, value="Temperaturzahl", landolt=F, weighted=TRUE, data=X , na.rm=FALSE, method="average"){

  value <- ifelse(landolt, paste(value, ".Landolt", sep=""), value) # Check if Landolt values are chosen
  if(!(value %in% names(data))) warning( paste('"', value, '" is not in data!', sep=""), immediate. = F, call. = TRUE)
  if(!(is.numeric(data[,value]))) {
     cat(paste('"', value, '" is not numeric!\n', sep="") )
    do.calculations <- F} else do.calculations <- T
  
  # subset the data
  rownames(data) <- data$Latin 
  data <- data[names(d),value]  # values in correct order
  
  r2 <- data.frame(species=names(d), value=data)
  
  if(do.calculations){
    if(weighted) d <- d/rowSums(d) else d[d>0 & !is.na(d)] <- 1  # when the weighted values are needed, make that plots add up to 1. Else make the data presence absence.
    R <- as.numeric()
    for(i in 1:nrow(d)){ # loop for each plot
      if(method=="average"){
        if(weighted) R[i] <- sum(t(d[i,])*data,na.rm=na.rm) else R[i] <- mean(t(d[i,])*data,na.rm=na.rm) # when weighted sum up the weighted components Else take the average of the occuring species
      }
      if(method=="sd"){ # if standard deviation is chosen
        if(weighted) R[i] <- sd(t(d[i,])*data,na.rm=na.rm) else R[i] <- sd(t(d[i,])*data,na.rm=na.rm)
      }
    }
    
    names(R) <- rownames(d)
    
    return(list(value=paste(ifelse(weighted, "weighted", ""), method, value), plots=R, species=r2))
    } else   return(list(value=value, species=r2))
  }
  
  
# Example
# species <- c("Daucus carota",  "Scorzoneroides autumnalis", "Silene latifolia", "Hypochaeris radicata")
# d <- data.frame(species, plotA= runif(length(x)), plotB= runif(length(x)), plotC= runif(length(x)))  # create random dataframe with species as rows and plots as columns
# rownames(d) <- d$species # make rownames
# d$species <- NULL # remove species column
# d <- as.data.frame(t(d))  # transpose dataframe

# get.indicator.value(d = d[,  ], value = "Feuchtezahl", landolt = T, data = X, weighted = T, na.rm = T)
# get.indicator.value(d = d[,  ], value = "Feuchtezahl", landolt = TRUE, data = X, weighted = T, method = "sd")

  
