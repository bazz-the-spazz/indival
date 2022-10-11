
# # Example

# 1. 
# Run the scripts to get the functions and the source data:
source("Calculate_indicator_values.r")
source("Correct_species_names.r")
source("read_data_from_Vegedaz.r")


# 1.1 
# read data from Vegedaz (Flora Indicativa, Landolt)
# path=("~/.wine/drive_c/Program Files/Vegedaz/Daten/") # on Linux
# path=("C:\Program Files\Vegedaz\Daten\") # or wherever Vegedaz is located on Windows
vegedaz <- read.vegedaz.data(path = "C:\Program Files\Vegedaz\Daten\")

# 1.2
# for Veg (Flora Helvetica 2014), extract the Zip file and put the file "Zeigerliste.txt" into your work directory.
source("read_data_from_Veg_2015.r")

# 1.3
## choose your data source to proceed
source <- vegedaz$indicativa   # the options are 'vegedaz$landolt', 'vegedaz$indicativa' from Vegedaz, or 'floraH' from Veg

# 2.
# for the exercise create a random dataframe (with species as rows and plots as columns)

# 2.1
species <- c("Daucus carota",  "Lathyrus pratensis", "Scorzoneroides autumnalis", "Aegopodiu podagraria", "Leucanthemum vulgaris", "Erigeron annuus")
d <- data.frame(species=species, plotA= runif(length(species)), plotB= runif(length(species)), plotC= runif(length(species)))
d

# 2.2 transpose the data.frame for the analyses (species as columns and plots as rows)
row.names(d) <- d$species
d$species <- NULL
d <- as.data.frame(t(d))
d

# 3. 
# Use the choose.name()-function to correct the species names according to the chosen source
corrected.names <- choose.name(names = names(d), data = source)
corrected.names
names(d) <- corrected.names
d

## if you're tired of choosing species names you can type 'pause' and later resume the task with: choose.name(names = names(d), data = source, continue.after.pause = corrected.names)

# 4. 
# Use the get.indicator.value()-function to calculate e.g. mean e.g. Temperaturzahl for the plots
indi.temp.mean <- get.indicator.value(d=d, value = "Temperaturzahl", data = source, socio = T, propose.alternatives = T, method="mean") # *
indi.temp.mean

# * The argument 'socio=TRUE' only works when vegedaz$indicativa is chosen as data source. It guesses the Vegetation type from the most common (wheighted) associated Vegetation type of all component species.

