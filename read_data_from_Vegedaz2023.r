## Get data from vegedaz23 (Beta-Version vom Februar 2023)
# path should lead to the DB folder in the VegedazQTrelease directory

read.vegedaz23.data <- function(path){

	artlist <- read.delim(paste(path,"Artlist.txt", sep=""), sep = "@", header = F)
	# variable names are specified in the first rows
	n <- do.call(rbind,strsplit(artlist[2:10,1], "\t"))[,2]

	artlist <- artlist[-1:-10,]

	# recreate data.frame
	artlist <- strsplit(artlist, "\t")
	artlist <- as.data.frame(do.call(rbind,artlist))

	names(artlist) <- n



	# Prepare Flora Indivativa
	zeilan <- read.delim(paste(path,"Zeilan.txt", sep=""), header = T)
	zeilan$X <- NULL

	# merge
	artlist$Latin <- artlist$Taxon_plain
	zeilan$Latin <- zeilan$plainName

	zeilan <-    merge(artlist, zeilan, by = "Latin", all=T)


	# use Data that has at least one Zeigerwert

	return(zeilan)
	# rm(artlist, ind, con, i, l, n)
}


d <- read.vegedaz23.data(path)
d$Latin
