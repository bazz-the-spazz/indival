

# Function to calculate weighted or mean indicator values
## data has to be in the form: columns are species, rows are plots, rownames are the names of the plots


get.indicator.value <- function(d, value="Temperaturzahl", weighted=TRUE, data , na.rm=TRUE, method="mean", socio=T, propose.alternatives=T){
	
	if(!(value %in% names(data))) warning( paste('"', value, '" is not in data!', sep=""), immediate. = F, call. = TRUE)
	if(!(is.numeric(data[,value]))) {
		cat(paste('"', value, '" is not numeric!\n', sep="") )
		do.calculations <- F} else do.calculations <- T
		
		# subset the data
		data.bak  <- X <- data
		rownames(data) <- data$Latin
		data <- data[names(d),value]  # values in correct order
		
		
		# for propose.alternatives in case of missing values
		if(propose.alternatives & TRUE %in% is.na(data) ){
			
			nams <- namso <-  names(d)[is.na(data)]
			nams <- paste(nams , " ") # add blank at the end
			nams <- gsub(" ", "  ", nams) # double all the blanks in the names
			nams <- gsub("cf\\.", "cf ", nams) # double all the blanks in the names
			nams <- gsub("sp\\.", "sp ", nams) # double all the blanks in the names
			for(i in c( "_", " cf ", " sp ", "   ", "  ", "  ")) nams <- gsub(i, " ", nams) # remove all the points, underscores, "cf", "sp", tripple and double blanks from the names
			nams <- substr(nams, start = 1, stop = nchar(nams)-1) # remove the blank from the end
			
			X$g <- sub(" .*", "", X$Latin) # get Genus of all Species
			X$s <- sub(" .*", "", substr(stop   = 100, start = nchar(X$g)+2, x = X$Latin))
			X$gs <- paste(X$g, X$s)
			
			
			
			for(i in 1:length(nams)){ #names(d)[is.na(data)]){ # loop for each species with with missing values
				
				
				#first get all entrys of the same species which have the data
				x <- X$gs[X$Latin==nams[i]]
				if(length(x)==0) {
					x <- X$gs[X$gs==nams[i]]
					if(length(x)==0) {
						x <- unique(X$gs[agrep(nams[i], X$gs)])
					}
				}
				
				alternative <- character()
				if(length(x)==1) alternative <- X[X$gs==x & !is.na(X[,value]), "Latin" ]
				if(length(x)>1) alternative <- X[X$g== sub(" .*", "", x)  & !is.na(X[,value]), "Latin" ]
					
					
				
				if(length(alternative)>0){
					I <- answer <- 0
					while(!(answer %in% 1:(length(alternative)+1) ) | answer==0 ){
						if(I>0) cat(paste("\n Only NUMBERS between 1 and",length(alternative)+1,"allowed.\n"))
						cat(paste("\n",namso[i], ": No value '", value, "'. Choose other species:\n",
											paste(
												paste(1:length(alternative), ". ", alternative, " (", X[X$Latin %in% alternative, value], ")\n", sep="")
												, collapse = ""),
											paste(length(alternative)+1, ". keep '", namso[i],"'.\n", sep = "" )
											, sep=""))
						answer <- readline(prompt = paste("Choose number between 1 and ",length(alternative)+1,":", sep=""))
						I <- 1
					}
					if(as.numeric(answer) <= length(alternative)) data[names(d)==namso[i]] <-  X[X$Latin==alternative[as.numeric(answer)], value]
				}
				
			}
		}
		
		
		# for sozio
		cols <- c("Pflanzengesellschaft_1_txt", "Pflanzengesellschaft_2_txt", "Pflanzengesellschaft_3_txt")
		if(unique(cols %in% names(data.bak))==F) socio <- F
		
		sozz <- function(nam, wheight, data=data.bak){
			# Function to look-up which Plant-Society is the most likely
			k <- as.character(as.matrix(data[data$Latin %in% nam, cols])) # all mentioned societies
			su <- sort(unique(k)) # unique societies
			D <- data.frame(Gesellschaft=su, Frequency=rep(0, length(su)))
			for(i in D$Gesellschaft){
				D$Frequency[D$Gesellschaft==i] <- length(which(k == i))  # look up the frequencies
			}
			
			D <- D[D$Gesellschaft!="",] # ignore those with no information
			
			if(nrow(D)>0){ # if there are no data for the community skip the rest
				#wheight the frequency by abundance
				if(!missing(wheight)){
					D$Frequency.w <- 0
					if(ncol(wheight)>1) wheight <- as.data.frame(t(wheight))
					for(i in nam){
						h <- as.character(t(data[data$Latin==i, cols]))
						h <- h[h!=""]
						for(j in h){
							D$Frequency.w[D$Gesellschaft==j] <- D$Frequency.w[D$Gesellschaft==j] + D[D$Gesellschaft==j, "Frequency"]*wheight[i,1]
						}
					}
					D$Frequency.old <- D$Frequency
					D$Frequency <- D$Frequency.w
					D$Frequency.w <- NULL
				}
				
				
				
				# If there is a tie for the most frequent society, choose the one that is highest in priority
				if(length(which(D$Frequency==max(D$Frequency))) >1){
					Favourites <- data.frame(name=D$Gesellschaft[D$Frequency==max(D$Frequency)], prio1=NA, prio2=NA, prio3=NA)
					rownames(Favourites) <- Favourites$name
					ddd <- data[data$Latin %in% nam, cols]
					for(i in Favourites$name){
						Favourites[i, "prio1"] <- length(which(ddd$Pflanzengesellschaft_1_txt==i))*2 #give a bonus to the highest priority
						Favourites[i, "prio2"] <- length(which(ddd$Pflanzengesellschaft_2_txt==i))
						Favourites[i, "prio3"] <- length(which(ddd$Pflanzengesellschaft_3_txt==i))*.5 #give a malus to the highest priority
					}
					Favourites$total <- Favourites$prio1+Favourites$prio2+Favourites$prio3 # total the bonuses
					for(i in Favourites$name) D[D$Gesellschaft==i, "Frequency"] <- D[D$Gesellschaft==i, "Frequency"] + Favourites[i, "total"]
				}
				D <- D[order(D$Frequency, decreasing = T),] # order
				
				return(D)
			} else return(data.frame(Gesellschaft="", Frequency=0, Frequency.w=0))
		}
		
		
		r2 <- data.frame(species=names(d), value=data)
		
		if(do.calculations){
			if(weighted) d <- d/rowSums(d) else d[d>0 & !is.na(d)] <- 1  # when the weighted values are needed, make that plots add up to 1. Else make the data presence absence.
			R <- as.numeric()
			D <- as.character()
			for(i in 1:nrow(d)){ # loop for each plot
				if(method=="mean"){
					if(weighted) R[i] <- sum(t(d[i,])*data,na.rm=na.rm) else R[i] <- mean(t(d[i,])*data,na.rm=na.rm) # when weighted sum up the weighted components Else take the average of the occuring species
				}
				if(method=="sd"){ # if standard deviation is chosen
					if(weighted) R[i] <- sd(t(d[i,])*data,na.rm=na.rm) else R[i] <- sd(t(d[i,])*data,na.rm=na.rm)
				}
				if(socio & method=="mean"){
					if(weighted) x <-sozz(nam = names(d)[t(d[i,])>0], wheight=d[i,], data = data.bak)
					if(!weighted) x <- sozz(nam = names(d)[t(d[i,])>0], data = data.bak)
					D[i] <- paste(x[x$Frequency==max(x$Frequency),1], collapse = "; ")
				}
			}
			
			names(R) <-  rownames(d)
			if(socio & method=="mean") names(D) <- rownames(d)
			
			
			
			Return <- (list(value=paste(ifelse(weighted, "weighted", ""), method, value), plots=R, species=r2))
			if(socio & method=="mean") Return <- append(Return, list(likely.Pflanzengesellschaft=D))
			
			
		} else   Return <- (list(value=value, species=r2))
		
		return(Return)
}

