# Function to reshuffle your data from where each column is one Plot, to each row equals one Plot.

transpose.data <- function(file, data){
  
  # First: read file, if data is missing. 
  if(missing(data)){
    if(length(grep(".xlsx", file, ignore.case = T))>0){
      require(openxlsx)
      data <- read.xlsx(file)
    } else {
      if(length(grep(".csv", file, ignore.case = T))>0){
        data <- read.csv(file)
      } else {
        data <- read.table(file)
      }
    }
   }
  
  # Transpose the data frame
  x <- names(data)[-1]
  n <- c(names(data)[1],data[,1])
  data <- data[,-1]
  data <- as.data.frame(cbind(x, t(data)))
  
  names(data) <- n
  rownames(data) <- 1:nrow(data)
  
  
  # Make those columns numeric, which only contain numbers (and points)
  for(i in 1:ncol(data)) {
    if( sort(unique(unlist(strsplit(paste(data[,i],collapse = ""), "")) %in% c(0:9, ".")))[1]==TRUE   ){
      data[,i] <- as.numeric(as.character(data[,i]))
    }
  }
  
  if("date" %in% names(data)) data$date <- as.Date(data$date, origin = "1899-12-30")
  
  return(data)
}

# Example
# d <- transpose.data(file = "vegetation_surveys_2022_R(1421).xlsx")

