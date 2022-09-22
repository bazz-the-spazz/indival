
## Get data from vegedaz
# on windows: setwd("C:\\Program files\Vegedaz\Data")

## Prepare Artlist
con <- file("Artlist.txt", encoding = "windows-1252" )
artlist <- readLines(con = con)
artlist <- artlist[-1]
# artlist[1]
close(con)

artlist <- strsplit(artlist, "\t")

# some species have only 48 columns, ad one at the end
l <- 0
for(i in 1:length(artlist)) l[i] <- length(artlist[[i]])
# artlist[which(l==49)[3]]
# artlist[which(l==48)[3]]

for(i in which(l==48)) artlist[[i]] <- c(artlist[[i]], "")

# artlist[[1]]
n <- sub(" .*", "", artlist[[1]])
n[n=="Latine"] <- "Latin"

artlist <- as.data.frame(do.call(rbind,artlist[-1]))
# dim(artlist)
names(artlist) <- n
artlist <- artlist[, c("Genus",    "Species",  "Author",   "Family" , "Deutsch",  "Français", "Italiano", "Latin",    "Wuchs" ,
                        "Rang", "AggrList")]

artlist$Name <- paste(artlist$Genus, artlist$Species)
artlist <- artlist[, c(ncol(artlist), 1:(ncol(artlist)-1))]


## Prepare Flora Indivativa 2019

con <- file("FloInd.txt", encoding = "windows-1252" )
ind <- readLines(con = con)
ind <- ind[-1] # get rid of first line (its used as formating)
# ind[1]
close(con)
ind <- strsplit(ind, "\t")


# some species have only 99 columns, ad one at the end
l <- 0
for(i in 1:length(ind)) l[i] <- length(ind[[i]])
# ind[which(l==100)[3]]
# ind[which(l==99)[3]]
for(i in which(l==99)) ind[[i]] <- c(ind[[i]], "")

# Column names
n <- sub(" .*", "", ind[[1]])

# make data.frame
ind <- as.data.frame(do.call(rbind,ind[-1]))
# dim(ind)
names(ind) <- n


## Read Landolt - Values,, "Zeigerwerte Landolt und NISM Mai 2019"
con <- file("ZeiLan.txt", encoding = "windows-1252" )
landolt <- readLines(con = con)
landolt <- landolt[-1] # get rid of first line
# landolt[1]
close(con)

landolt <- strsplit(landolt, "\t")


# some species have only 11 columns, ad one at the end
l <- 0
for(i in 1:length(landolt)) l[i] <- length(landolt[[i]])
#  landolt[which(l==12)[1]]
#  landolt[which(l==11)[1]]
for(i in which(l==11)) landolt[[i]] <- c(landolt[[i]], "")

# Column names
n <- sub(" .*", "", landolt[[1]])

# make data.frame
landolt <- as.data.frame(do.call(rbind,landolt[-1]))
# dim(landolt)
names(landolt) <- n
names(landolt)[2:ncol(landolt)] <- paste(names(landolt)[2:ncol(landolt)] , ".Landolt", sep="")


d <- merge(artlist, ind, by.x = "Latin", by.y = "Species", all=T)
dd<- merge(d, landolt, by.x = "Latin", by.y = "Species", all=T)

# # Make all the Zahl-Variables numeric

dd$Feuchtezahl <- as.numeric(gsub("-",NA,dd$Feuchtezahl))
dd$Naehrstoffzahl <- as.numeric(gsub("-",NA,dd$Naehrstoffzahl))
dd$Wechselfeuchtezahl <- as.numeric(gsub("-",NA,dd$Wechselfeuchtezahl))
dd$Reaktionszahl <- as.numeric(gsub("-",NA,dd$Reaktionszahl))
dd$Humuszahl <- as.numeric(gsub("-",NA,dd$Humuszahl))
dd$Durchlueftungszahl <- as.numeric(gsub("-",NA,dd$Durchlueftungszahl))
dd$Salzzahl <- as.numeric(gsub("\\?","",dd$Salzzahl))
for( i in grep("zahl", names(dd))){
  dd[,i] <- as.numeric(dd[,i])
}

write.xlsx(dd, "Merged_Artenliste_FloInd_ZeiLan_2019.xlsx")

# use Data that has at least one Zeigerwert
dd <- dd[ which(rowSums(dd[,grep("zahl", names(dd))], na.rm = T) > 0), ]
write.xlsx(dd, "Merged_Artenliste_FloInd_ZeiLan_2019_reduced.xlsx")

