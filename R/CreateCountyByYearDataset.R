# ----- County by Year Function -----------------------------------------------

# TODO: Create function that when given the appropriate Census.gov URL, returns 
# a dataframe with 3 and 5 digit county FIPS, county name and total population by
# year. 


library(maps)
library(stringr)
library(reshape2)

# TODO:  Document that this ONLY works for a couple of datasets from a specific URL
CountyByYearCensus <- function(URL='http://www.census.gov/popest/data/counties/asrh/pre-1980/tables/co-asr-7079.csv'){

  # ----- Step 1: read and tidy data from URL ---------------------------------  
  
  file <- url(URL)
  col_types <- 'iciiiiiiiiiiiiiiiiiii'
  # Create age division names for this data
  a <- seq(0,85,5)
  b <- seq(4,89,5)
  divisions <- paste0('age',a,'_',b)
  col_names <- c('year', 'fipsCode', 'raceCode', divisions)
  # Read in raw data and create standard column names (based on website, we are assuming 
  # all the files will be standardized)
  df <- readr::read_csv(file, col_names=FALSE, col_types=col_types)
  names(df) <- col_names 
  
  # creating race_gender factor
  levelNames <- c('White Male', 'White Female', 'Black Male', 'Black Female', 'Other Race Male', 'Other Race Female')
  df$race_gender <- factor(df$raceCode, levels=1:6, labels=levelNames)
  

  
  # ----- COME UP WITH GOOD DESCRIPTION FOR THIS CHUNK ------------------------
    
  # Add stateISO variable to the census dataframe
  load('Data/CountyFIPS.StateISO_datadict.RData')
  df$stateISO <- countyFips[df$fipsCode, 'stateISO']
  
  
  # ----- Creating simple dataset of total population by county by year ---------
  
  # reshapedDF dataframe that contains total county population organized in county~year fashion
  # which we will then seperate into unique years and attach to spatial polygons (??) 
  
  
  # Initialize vectors to store values as we loop through the census dataset 6-rows at a time
  totPop <- vector('numeric', nrow(df)/6)
  countyFipsCode <- vector('character', nrow(df)/6)
  year <- vector('numeric', nrow(df)/6)
  
  index <- 1
  for (i in seq(1, nrow(df), 6)) {
    totPop[index] <- sum(df[i:(i+5), 4:21])
    countyFipsCode[index] <- df$fipsCode[i]
    year[index] <- df$year[i]
    index <- index + 1   
  }
  
  ###totalPopulation_long <- cbind(year, totPop, countyFipsCode)
  ###totalPopulation_long <- as.data.frame(totalPopulation_long)
  totalPopulation_long <- data.frame(year, totPop, countyFipsCode)
  totalPopulation_long$countyFips3 <- str_sub(totalPopulation_long$countyFipsCode, start=-3)
  melted <- melt(totalPopulation_long, id.vars=c('year', 'countyFipsCode'), measure.vars = 'totPop', factorsAsStrings=FALSE)
  
  totalPopulation <- dcast(melted, countyFipsCode~year)
  totalPopulation$StateFIPS <- str_sub(totalPopulation$countyFipsCode, end=-4)
  rownames(totalPopulation)<- as.character(totalPopulation$countyFipsCode)
  
  return(totalPopulation)
  

  
}













