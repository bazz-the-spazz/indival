

# Function to calculate weighted or mean indicator values
## data (abundance or presence/absence (0, 1)) has to be in the form: columns are species, rows are plots. Optionally: rownames are the names of the plots


get.indicator.value <- function(d, value="Temperaturzahl", weighted=TRUE, data , na.rm=TRUE, method="mean", socio=F, propose.alternatives=T, propose.alternatives.full=F, stetigkeit=FALSE, diversities=F){

	if(!(value %in% names(data))) warning( paste('"', value, '" is not in data!', sep=""), immediate. = F, call. = TRUE)
	if(!(is.numeric(data[,value]))) {
		cat(paste('"', value, '" is not numeric!\n', sep="") )
		do.calculations <- F} else do.calculations <- T

		# subset the data
		data.bak  <- X <- data
		rownames(data) <- data$Latin
		data <- data[names(d),value]  # values in correct order

		# change NAs to zero
		d[is.na(d)] <- 0


		if(diversities) d.diversity.backup <- d # special backup for diversity

		# for propose.alternatives in case of missing values
		N <- names(d)
		if((propose.alternatives | propose.alternatives.full) & TRUE %in% is.na(data) ){

			nams <- namso <-  names(d)[is.na(data)]
			if(propose.alternatives.full==FALSE)  nams <- namso <- nams[nams %in% data.bak$Latin]

			if(length(nams)>0){

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
					if(length(x)>1) alternative <- X[X$g %in% sub(" .*", "", x)  & !is.na(X[,value]), "Latin" ]



					if(length(alternative)>0){
						I <-  0
						answer <- -1
						while(!(answer %in% 1:(length(alternative)+1) ) | answer==-1 ){
							if(I>0) cat(paste("\n Only NUMBERS between 1 and",length(alternative)+1,"allowed.\n"))
							cat(paste("\n",namso[i], ": No value '", value, "'. Choose other species:\n",
												paste(
													paste(1:length(alternative), ". ", alternative, " (", X[X$Latin %in% alternative, value], ")\n", sep="")
													, collapse = ""),
												paste(length(alternative)+1, ". keep '", namso[i],"'.\n", sep = "" )
												, sep=""))
							answer <- readline(prompt = paste("Choose number between 1 and ",length(alternative)+1," (or zero):", sep=""))
							I <- 1
							if(answer %in% c("zero", 0) ) answer <- length(alternative)+1
						}
						if(as.numeric(answer) <= length(alternative)) {
							data[names(d)==namso[i]] <-  X[X$Latin==alternative[as.numeric(answer)], value]
							N[N==namso[i]] <- alternative[as.numeric(answer)]
						}
					}

				}
			}
		}



		r2 <- data.frame(species=names(d), value=data)
		if(!identical(names(d), N)) r2$used.species <- N # if you used alternatives, report them

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

					names(wheight) <- nam
					for(i in nam[which(duplicated(nam))]) {
						wheight[,which(names(wheight)==i)[1]] <- max(wheight[,names(wheight)==i])
						wheight <- wheight[,-which(names(wheight)==i)[2]]
					}
					wheight <- as.data.frame(t(wheight))
					nam <- unique(nam)

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

		## Function to calculate diversities
		diversitee <- function(x, q, margin=1, effective=TRUE, na.rm=TRUE, se=F, allow.incorrect.beta=FALSE){
			# based on Jost 2006 & 2007 calculate the (effective) diversity of a community. Loosely adapted from the diversity function in vegan.

			# Organize data
			x <- drop(as.matrix(x))
			if (!is.numeric(x)) stop("input data must be numeric")
			if (any(x < 0, na.rm = TRUE)) stop("input data must be non-negative")

			# Function to Calculate Effective Diversity
			f <- function(x, Q, na=na.rm, raw=FALSE){

				# Get Proportional Data
				x <- x[x>0] # Zeros are ignored
				if(na) x <- x[!is.na(x)]
				p <- x/sum(x)

				if(NA %in% p){
					d <- NA
				} else {
					if(Q==1){
						d <- exp(-sum(p*log(p))) #q=1 is not definded, calculate Shannon
						if(raw) d <- -sum(p*log(p)) # needed for Alpha
					} else {
						d <- sum(p^Q)^(1/(1-Q)) # Else use Jost's universal formula
					}
				}
				return(d)
			}


			# Apply the before defined function f to the data(frame)
			if(length(dim(x))>1){
				D <- apply(x, MARGIN = margin, FUN = function(x)f(x, Q=q) )
			} else {
				D <- f(x,Q=q)
			}


			# From Jost 2007 :PARTITIONING DIVERSITY INTO INDEPENDENT ALPHA AND BETA COMPONENTS
			# True Alpha diversity
			if(length(dim(x))>1){
				## First Calculate weights
				w <- apply(x, MARGIN = margin, FUN = function(x)f(x, Q = 0))
				w <- w/sum(w) # w is the statistical weight of Community j (usually the number of individuals in Community j divided by the total number of individuals in the region)

				if(q!=1){
					if(length(unique(w))==1 | allow.incorrect.beta){ # if weights w are not equal only Shannon might be used!
						sums <- D^(1/(1/(1-q)))
						A <- ( sum((w^q * sums))/ sum(w^q) )^(1/(1-q)) # eq 11a
					} else {
						A <- NA
					}
				}
				if(q==1){ # use formula 11b for q=1 (Shannon)
					lamdas <- apply(x, MARGIN = margin, FUN = function(x)f(x, Q=q, raw=TRUE) )
					A <- exp(sum(w*lamdas))
				}

				# Gamma? the numbers equivalent of the diversity index of the pooled samples
				G <- f( t(apply(x, MARGIN = ifelse(margin==1,2,1) , sum, na.rm=T)) ,Q=q)
				# Beta (Following from eq9)
				B <- G/A



				# Standard Error

				if(se & (q==1 | allow.incorrect.beta)){
					se.a <- se.b <- se.g <- numeric()
					for(i in 1:(nrow(x)-1)){
						for(j in (i+1):nrow(x)){
							xx <- x[c(i,j),]
							w <- apply(xx, MARGIN = margin, FUN = function(x)f(x, Q = 0))
							w <- w/sum(w)
							se.a <-  c(se.a, exp(sum(w*apply(xx, MARGIN = margin, FUN = function(x)f(x, Q=q, raw=TRUE) ))))
							se.g <- c(se.g, f( t(apply(xx, MARGIN = ifelse(margin==1,2,1) , sum, na.rm=T)) ,Q=q))
							se.b <- c(se.b, se.g[length(se.g)]/se.a[length(se.a)])
						}
					}
					se.a <- sd(se.a)/sqrt(ifelse(margin==1, nrow(x), ncol(x)))
					se.b <- sd(se.b)/sqrt(ifelse(margin==1, nrow(x), ncol(x)))
					se.g <- sd(se.g)/sqrt(ifelse(margin==1, nrow(x), ncol(x)))
				}



			}


			# Transform to non-effective Indices (if needed)
			if(effective==FALSE){
				if(q==0){
					D <- D
					I <- "Species Richness"
				}
				if(q==1){
					D <- log(D)
					I <- "Shannon entropy"
				}
				if(q==2){
					D <- 1-(1/D)
					I <- "Gini-Simpson index"
				}
				if(!(q %in% 0:2)) {
					Value <- NA
					I <- NA
				}
				return(list(Value=D, index=I, gamma=G, alpha=A, beta=B))
			} else {
				if(se){
					return(list(D=D, gamma=G, alpha=A, beta=B, se.g=se.g, se.a=se.a, se.b=se.b))
				} else {
					return(list(D=D, gamma=G, alpha=A, beta=B))
				}
			}
		}





		if(do.calculations){
			if(weighted) d <- d/rowSums(d, na.rm = T) else d[d>0 & !is.na(d)] <- 1  # when the weighted values are needed, make that plots add up to 1. Else make the data presence absence.
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
					if(weighted) x <-sozz(nam = N[t(d[i,])>0], wheight=(d[i,t(d[i,])>0]), data = data.bak)
					if(!weighted) x <- sozz(nam = N[t(d[i,])>0], data = data.bak)
					D[i] <- paste(x[x$Frequency==max(x$Frequency),1], collapse = "; ")
				}
			}

			names(R) <-  rownames(d)
			if(socio & method=="mean") names(D) <- rownames(d)



			Return <- (list(value=paste(ifelse(weighted, "weighted", ""), method, value), plots=R, species=r2))
			if(socio & method=="mean") Return <- append(Return, list(common.Pflanzengesellschaft=D))

			if(stetigkeit & socio){
				dd <- d
				dd[is.na(dd)] <- 0
				dd[dd>0] <- 1
				stetigkeit <- colSums(dd)/nrow(dd)
				Return <- append(Return, list(stetigkeit=stetigkeit))
			}

		} else   Return <- (list(value=value, species=r2))

		if(diversities){
			sr <- diversitee(x = d.diversity.backup, q = 0)$D
			Return <- append(Return, list("Species richness"=sr))
			if(weighted){
				esh <- diversitee(x = d.diversity.backup, q = 1)
				Return <- append(Return, list("effective Shannon diversity"=esh$D, "Beta diversity"=esh$beta))
			}
		}



		return(Return)
}

