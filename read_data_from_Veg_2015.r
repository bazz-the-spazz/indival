## Get data from the application "VEG - Bearbeiten von Vegetationstabellen"
# It contains the indicator values from Flora Helvetica 2015 in the file "Zeigerliste.txt"
# https://www.maerki.com/maerki_informatik/veg/index.html
getVegData <- function(path) {
	if(missing(path)) path <- ""
	con <- file(paste(path,"Zeigerliste.txt", sep=""), encoding = "windows-1252")
	zeigerliste <- readLines(con = con)
	close(con)
	n <- zeigerliste[21]
	zeigerliste <- zeigerliste[25:length(zeigerliste)]
	
	zeigerliste <- as.data.frame(do.call(rbind, strsplit(zeigerliste, "\t")))[,-1:-2]
	names(zeigerliste) <- unlist(strsplit(n, "\t"))[c(3:21)]
	names(zeigerliste)[names(zeigerliste)=="Name nach Flora Helvetica"] <- "Latin"
	
	for(i in grep("zahl", x = names(zeigerliste))) zeigerliste[,i] <- as.numeric(zeigerliste[,i])
	zeigerliste$Wurzeltiefe <- as.numeric(zeigerliste$Wurzeltiefe)
	floraH <- zeigerliste
	return(floraH)
}
