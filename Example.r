
# # Example

# 1.
# Run the scripts to get the functions and the source data:
source("Calculate_indicator_values.r")
source("Correct_species_names.r")
source("read_data_from_Vegedaz.r")
# source("read_data_from_Vegedaz2023.r") # not yet
source("read_data_from_Veg_2015.r")
source("transpose_data.r")

# 1.1
# read data from Vegedaz 2021 (Flora Indicativa, Landolt)
# path=("~/.wine/drive_c/Program Files/Vegedaz/Daten/") # on Linux
path=("C:/Program Files/Vegedaz/Daten/") # or wherever Vegedaz is located on Windows
vegedaz21 <- read.vegedaz.data(path)

# 1.2 not yet ready
# read data from Vegedaz 2023 (Landolt)
# path=(".../VegedazQtRelease_6/DB/") # or wherever VegedazQtRelease_6 is located on Windows
# vegedaz23 <- read.vegedaz23.data(path)


# 1.3
# for Veg (Flora Helvetica 2014), extract the Zip file and put the file "Zeigerliste.txt" into your work directory.
floraH <- getVegData(path="")

# 1.4 
## choose your data source to proceed
source <- vegedaz21$indicativa   # the options are 'vegedaz21$landolt', 'vegedaz21$indicativa' from Vegedaz21, 'vegedaz23' (not yet), or 'floraH' from Veg

# 2.
# for the exercise create a random dataframe (with species as rows and plots as columns)

# 2.1
species <- c("Daucus carota",  "Lathyrus pratensis", "Scorzoneroides autumnalis", "Aegopodium podagraria cf", "Heracleum sphondyllum sp", "Erigeron annuus")
d <- data.frame(species=species, plotA= runif(length(species)), plotB= runif(length(species)), plotC= runif(length(species)))
d

# 2.2 transpose the data.frame for the analyses (species as columns and plots as rows)
d <- transpose.data(data = d)
rownames(d) <- d[,1]  # plots as rownames
d <- d[,-1]						# Remove plot column
d


# 3.
# Use the choose.name()-function to correct the species names according to the chosen source
corrected.names <- choose.name(names = names(d), data = source)
corrected.names

## if you're tired of choosing species names you can type 'pause' and later resume the task with:
# choose.name(names = names(d), data = source, continue.after.pause = corrected.names)


# 4.
# Use the get.indicator.value()-function to calculate e.g. mean e.g. Temperaturzahl for the plots
indi.temp.mean <- get.indicator.value(
	data=d,
	corrected.names = corrected.names,
	value = "Temperaturzahl",
	weighted = T,
	source = source,
	na.rm = T,
	propose.alternatives = T)

indi.temp.mean


# 'data=' is your data.frame, column names are the corrected species names and the columns only contain numbers.
# 'corrected.names=' the string of names received from the choose.name-function in the same order as the columns of data. (optional)
# 'value=' is the indicator value you are interested in. Need not to be numeric. There are many options. Check out names(vegedaz$indicativa).
# 'weighted=' should the indicator value be weighted by the abundance of the species?
# 'source='  is the data that contains the indicator value for each species.
# 'na.rm=' should missing indicator values be ignored?
# 'method=' do you want the average value ("mean") or the standard deviation ("sd")?
# 'propose.alternatives=TRUE' sometimes a indicator value is missing but another subspecies might have it.
# 'propose.alternatives.full=TRUE' similar to the choose.name-function, the algorhythm is looking for similar sounding species.
# 'socio=TRUE' only works when vegedaz$indicativa is chosen as data source. It guesses the Vegetation type from the most common (wheighted) associated Vegetation type of all component species.
# 'stetigkeit=TRUE' the "Stetigkeit" is calculated.
# 'diversities = TRUE' Species richness, effective Shannon diversity, and Beta-Diversity are calculated.
