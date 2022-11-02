# Function to align your Species with Vegedaz (or others)

# Look for correct names
# function to choose "real" name of all species (with subsp. / aggr. / author and all that Jazz!)
## You can write 'pause' to pause the process and proceed by inserting the obtained list with 'continue.after.pause'

choose.name <- function(names, data=X, write.tmp.file=T, continue.after.pause){
	X <- data
	X$g <- sub(" .*", "", X$Latin) # get Genus of all Species
	pause <- F

	if(missing(continue.after.pause)) newnames <- as.character() else newnames <- continue.after.pause

	for(i in names[(length(newnames)+1):length(names)]){ # loop for all species
		if(pause==F){
			proceed <- FALSE
			if( i %in% X$Latin	) {  # if the name is already correct, add it to the new names list
				newnames[length(newnames)+1] <- i
			} else {
				i.back <- i # creat backup
				x <- character()  # x will be the new name
				while(length(x)==0 & !(i %in% c("pause", "NA", as.character(0:9))) ){ # keep asking Names  till there is a good suggesetion or name is "NA" or a number (0-9) or "pause"
					g <-  sub(" .*", "", i)
					if(!(g %in% X$g)) { # if genus not found compare to all species
						x <- agrep(i, X$Latin, value = TRUE, ignore.case = TRUE)
						cat(paste('Genus "', g, '" not found\n', sep = ""))
						if(length(x)==1) cat(paste('"', i, '" was corrected to "',x, '"\n', "\n", sep = "" ))
					} else { #if genus is found, search only within genus
						x <- agrep(i, X$Latin[X$g==g], value = TRUE, ignore.case = TRUE)
						if(length(x)==1) cat(paste('"', i, '" was corrected to "',x, '"\n', "\n", sep = "" ))
					}
					if(length(x)==0) {
						cat(paste('"', i, '" was not found in List.\n', sep=""))
						i <- readline('Change name:')# if no similar name is found, ask for a total new name
					}
				}

				if(i %in% c("NA", as.character(0:9))) x <- i.back # when name was "NA" or number, put in original name



				if(length(x)>1) { # if there are option, ask for a choice
					cat(paste('"',i, '" was not found in List. Alternatives:\n', paste(paste(1:length(x),x, sep = " - "), collapse = '\n'), sep = ""))
					nr <- readline(prompt = paste('Choose number:' ,'', sep=""))
					if(nr=="pause") pause <- T else  {
						z <- x[as.numeric(nr)]
						cat(paste('"', i, '" was corrected to "',z, '"\n', "\n", sep = "" ))
					}
				} else {
					z <- x
				}

				if(pause) print("Pause!") else {
					newnames[length(newnames)+1] <- z
				}

			}
		}
	}

	if(write.tmp.file) write.csv(data.frame(oldnames=names[1:length(newnames)], newnames=newnames), "tmp.csv")

	return(newnames)
}

# test <- c("Daucus carota",  "Scorzoneroides autumnalis", "Silene latifolia", "Hypochaeris radicata")
# x <- choose.name(names=test)
# x
