

# Function to calculate weighted or mean indicator values
## data has to be in the form: columns are species, rows are plots, rownames are the names of the plots


get.indicator.value <- function(d, value="Temperaturzahl", weighted=TRUE, data , na.rm=FALSE, method="average", socio=T){
	
	if(!(value %in% names(data))) warning( paste('"', value, '" is not in data!', sep=""), immediate. = F, call. = TRUE)
	if(!(is.numeric(data[,value]))) {
		cat(paste('"', value, '" is not numeric!\n', sep="") )
		do.calculations <- F} else do.calculations <- T
		
		# subset the data
		data.bak <- data
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
			
			
			
	 Return <- (list(value=paste(ifelse(weighted, "weighted", ""), method, value), plots=R, species=r2))
			
			
		} else   Return <- (list(value=value, species=r2))
		
		
		# look for the most common soziology
		if(socio & "Pflanzengesellschaft_1_txt" %in% names(data.bak)){
			cols <- c("Pflanzengesellschaft_1_txt", "Pflanzengesellschaft_2_txt", "Pflanzengesellschaft_3_txt")
			# Function to look-up which Plant-Society is the most likely
			nam <- r2$species
			k <- character() # all mentioned societies
			for(i in length(cols)) k <- c(k, data.bak[data.bak$Latin %in% nam,cols[i]])
			su <- sort(unique(k)) # unique societies
			D <- data.frame(Gesellschaft=su, Frequency=rep(0, length(su)))
			for(i in D$Gesellschaft){
				D$Frequency[D$Gesellschaft==i] <- length(which(k == i))  # look up the frequencies
			}
			
			D <- D[D$Gesellschaft!="",] # ignore those with no information
			D <- D[order(D$Frequency, decreasing = T),] # order
			
			
			Return <- append(x = Return, values = list(Pflanzengesellschaft=D))
		}
		
		
		
		return(Return)
}


# # Example
# species <- c("Daucus carota",  "Scorzoneroides autumnalis", "Silene latifolia", "Hypochaeris radicata")
# d <- data.frame(species=species, plotA= runif(length(species)), plotB= runif(length(species)), plotC= runif(length(species)))  # create random dataframe with species as rows and plots as columns
# rownames(d) <- d$species # make rownames
# d$species <- NULL # remove species column
# d <- as.data.frame(t(d))  # transpose dataframe

# get.indicator.value(d = d[,  ], value = "Temperaturzahl", data = indicativa, weighted = T, na.rm = T)
# get.indicator.value(d = d[,  ], value = "Temperaturzahl", data = indicativa, weighted = T, method = "sd")
