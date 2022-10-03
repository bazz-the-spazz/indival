## Get data from vegedaz
# # on windows:
# setwd("C:\Program Files\Vegedaz\Daten\") ?
# # on Linux:
# setwd("~/.wine/drive_c/Program Files/Vegedaz/Daten/")


# Prepare Artlist
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


# Prepare Flora Indivativa

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


# Read Landolt - Values,, "Zeigerwerte Landolt und NISM Mai 2019"
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
# names(landolt)[2:ncol(landolt)] <- paste(names(landolt)[2:ncol(landolt)] , ".Landolt", sep="")


indicativa <- merge(artlist, ind,     by.x = "Latin", by.y = "Species", all.y=T, all.x=F)
landolt <-    merge(artlist, landolt, by.x = "Latin", by.y = "Species", all=T)

# # Make all the Zahl-Variables numeric
indicativa$Feuchtezahl <- as.numeric(gsub("-",NA,indicativa$Feuchtezahl))
indicativa$Naehrstoffzahl <- as.numeric(gsub("-",NA,indicativa$Naehrstoffzahl))
indicativa$Wechselfeuchtezahl <- as.numeric(gsub("-",NA,indicativa$Wechselfeuchtezahl))
indicativa$Reaktionszahl <- as.numeric(gsub("-",NA,indicativa$Reaktionszahl))
indicativa$Humuszahl <- as.numeric(gsub("-",NA,indicativa$Humuszahl))
indicativa$Durchlueftungszahl <- as.numeric(gsub("-",NA,indicativa$Durchlueftungszahl))
indicativa$Salzzahl <- as.numeric(gsub("\\?","",indicativa$Salzzahl))
for( i in grep("zahl", names(indicativa))){
  indicativa[,i] <- as.numeric(indicativa[,i])
}

for( i in grep("zahl", names(landolt))){
  landolt[,i] <- as.numeric(landolt[,i])
}

# use Data that has at least one Zeigerwert
indicativa <- indicativa[ which(rowSums(indicativa[,grep("zahl", names(indicativa))], na.rm = T) > 0), ]
landolt <- landolt[ which(rowSums(landolt[,grep("zahl", names(landolt))], na.rm = T) > 0), ]


rm(artlist, ind, con, i, l, n)
