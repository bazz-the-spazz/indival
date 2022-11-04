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
				i <- gsub("\\."," ", i) # get rid of points
				i <- gsub("_"," ", i)   # get rid of underscores
				i <- paste(i, " ", sep="") # add a blank at the end
				i <- gsub("  "," ", i)   # get rid of double blanks

				cf <- sp <- F
				if(length(grep(" cf ", i, ignore.case = T))>0) cf <- T
				if(length(grep(" sp ", i, ignore.case = T))>0) sp <- T

				i <- gsub(" cf "," ", i)   # get rid of cf
				i <- gsub(" sp "," ", i)   # get rid of sp

				x <- character()  # x will be the new name

				toggle <- F # a switch for when Species name is changed

				while(length(x)==0 & !(i %in% c("pause", "zero", "NA", as.character(0:9))) ){ # keep asking Names  till there is a good suggesetion or name is "NA" or a number (0-9) or "pause"
					if(toggle) i.back <- i  # change species name when switch is on
					g <-  sub(" .*", "", i)

					if(!(g %in% X$g)) { # if genus not found compare to all species
						x <- agrep(i, X$Latin, value = TRUE, ignore.case = TRUE)
						cat(paste('Genus "', g, '" not found\n', sep = ""))
						if(length(x)==1) cat(paste('"', i.back, '" was corrected to "',x, '"\n', "\n", sep = "" ))
					} else { #if genus is found, search only within genus
						x <- agrep(i, X$Latin[X$g==g], value = TRUE, ignore.case = TRUE)
					}

					if(length(x)==0) { # when species was not found, ask for a new name
						cat(paste('"', i.back, '" was not found in List.\n', sep=""))
						i <- readline('Enter a new name (zero to keep original):')# if no similar name is found, ask for a total new name
						if(!(i %in% c("zero", 0, "NA"))) toggle <- T  # toggle the switch when a new name is typed in
					}
				}

				# when name was "NA" or number, put in original name
				if(i %in% c("NA", "zero", as.character(0:9))) x <- i.back

				# When only one option: check if there is "cf" or "sp" in the name before automatically correcting.
				if(length(x)==1) {
					if(cf){ # if there is a cf
						cat(paste('\n"cf" detected in "',i.back, '". What do we do?', '\n1. Change to "', x, '?\n2. Keep "', i.back, '"?', sep="" ))
						nr <- 0
						while(!(nr %in% 1:2)) {
							nr <- readline(prompt = paste('Choose number:' ,'', sep=""))
							if(!(nr %in% 1:2))	cat("Sorry, do it again!")
						}
						x <- ifelse(nr==1, x, i.back)
					}
					if(sp){  # if there is a sp
						cat(paste('\n"sp" detected in "',i, '". What do we do?', '\n1. Change to "', x, '?\n2. Keep "', i.back, '"?', sep="" ))
						nr <- 0
						while(!(nr %in% 1:2)) {
							nr <- readline(prompt = paste('Choose number:' ,'', sep=""))
							if(!(nr %in% 1:2))	cat("Sorry, do it again!")
						}
						x <- ifelse(nr==1, x, i.back)
					}
					if(x!=i.back)cat(paste('"', i.back, '" was corrected to "',x, '"\n', "\n", sep = "" ))
				}


				if(length(x)>1) { # if there are option, ask for a choice
					cat(paste('"',i.back, '" was not found in List. Alternatives:\n', paste(paste(1:length(x),x, sep = " - "), collapse = '\n'), sep = ""))
					nr <- ""
					while(!(nr %in% c("zero","NA",0:length(x)))) {
						nr <- readline(prompt = paste('Choose number (zero to keep original):' ,'', sep=""))
						if(!(nr %in% c("zero","NA",0:length(x))))	cat("Sorry, do it again!")
					}

					if(nr=="pause") pause <- T else  {
						if(nr %in% c("zero", 0)){
							z <- i.back
							cat(paste('"', i.back, '" was kept.\n', "\n", sep = "" ))
						} else {
							z <- x[as.numeric(nr)]
							cat(paste('"', i, '" was corrected to "',z, '"\n', "\n", sep = "" ))
						}
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
