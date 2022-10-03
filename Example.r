
# # Example

#1. Run the scripts to get the source data:

# from Vegedaz (Flora Indicativa, Landolt)
setwd("C:\Program Files\Vegedaz\Daten\") # or wherever Vegedaz is located
source("read_data_from_Vegedaz.r")

# for Veg (Flora Helvetica 2014), extract the Zip file and put the file "Zeigerliste.txt" into your work directory.
source("read_data_from_Veg_2015.r")


#2.1 for the excecise i create a random dataframe (with species as rows and plots as columns)

species <- c("Daucus carota",  "Scorzoneroides autumnalis", "Silene latifolia", "Hypochaeris radicata")
d <- data.frame(species=species, plotA= runif(length(species)), plotB= runif(length(species)), plotC= runif(length(species)))
d

#2.2 transpose data.frame for the analyses (species as columns and plots as rows)

row.names(d) <- d$species
d$species <- NULL
d <- as.data.frame(t(d))

#3. Use the choose.name()-function to correct the species names

## choose your data source
source <- indicativa # all options are 'landolt', 'indicativa', 'floraH'

corrected.names <- choose.name(names = names(d), data = source) 
names(d) <- corrected.names

#4. Use the get.indicator.value()-function to calculate mean e.g. Temperaturzahl for the three plots
get.indicator.value(d=d, value = "Temperaturzahl", data = source)
get.indicator.value(d=d, value = "Temperaturzahl", data = source, method = "sd")
