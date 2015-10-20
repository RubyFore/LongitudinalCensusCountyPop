# ----- Parsing County Population Data

# # ----- Creating Fips-Name conversion table -----------------------------------

# NOTE:  We need a table to match FIPS codes to state and county names
# NOTE:  A good start exists in the 'maps' package but they have a funky
# NOTE:  mapping onto polygons names like "washington,san juan:lopez".
# NOTE:  So we need to clean this up and then append some other colunms
# NOTE:  to get a really useful dataframe.  Here we go:

# TODO:  

# Load maps::county.fips
library(maps)
library(stringr)
library(reshape2)

data(county.fips)

# Clean up funky names
county.fips$polyname <- str_replace(county.fips$polyname, ':.*$', '')
nameMatrix <- str_split_fixed(county.fips$polyname, ',', 2)
nameMatrix <- as.data.frame(nameMatrix)
# Add clean names back into dataframe
countyFips <- cbind(county.fips, nameMatrix)
countyFips <- countyFips[,c(1,3,4)]
countyFips <- dplyr::distinct(countyFips, fips)
names(countyFips) <- c('fips', 'stateName', 'countyName')
# Create sepearated county and state Codes 
countyFips$countyCode <- str_sub(countyFips$fips, -3)
countyFips$stateCode <- str_sub(countyFips$fips, end=-4)

# creating singleMask to eliminate single character state codes 

singleMask <- as.integer(countyFips$stateCode) < 10

countyFips[singleMask,'stateCode'] <- paste0('0', countyFips[singleMask, 'stateCode'])
countyFips[singleMask,'fips'] <- paste0('0', countyFips[singleMask, 'fips'])

#Add state ISO code
stateISOtoFips <- read.csv('/Users/rubyfore/Data/CountyPopulation/StateISOtoFips.csv',stringsAsFactors=FALSE)
stateISOtoFips$Numeric.code[c(1,2,4,5,6,8,9,10,11)] <- paste0('0',stateISOtoFips$Numeric.code[c(1,2,4,5,6,8,9,10,11)])
rownames(stateISOtoFips) <- as.character(stateISOtoFips$Numeric.code)
countyFips$stateISO <- stateISOtoFips[countyFips$stateCode,'Alpha.code']

#NOTE: to accomplish what we accomplish in lines above, I originally tried to use 
# nested for loops, but this was VERY slow. So instead we assigned the county fips codes 
# to be row names and then subsetted based on these row names to create a new column 
# in our dataset. 

# To sample a vector, matrix or data frame, by the particular values of a column it is 
# convenient to assign names/row names and then pass in a vector that references these rownames 
# by subsetting. 



# -----Working with Census Dataset from website -------------------------------


# File Format Description

# Meta data URl: 
# https://www.census.gov/popest/data/counties/asrh/pre-1980/tables/co-asr-7079-layout.txt

# Column  Description
# 
# 1 	Year
# 2 	State and County FIPS Codes
# 3 	Race/Sex code
# 4	0-4 year olds
# 5	5-9 year olds
# 6	10-14 year olds
# 7	15-19 year olds
# 8	20-24 year olds
# 9	25-29 year olds
# 10	30-34 year olds
# 11	35-39 year olds
# 12	40-44 year olds
# 13	45-49 year olds
# 14	50-54 year olds
# 15	55-59 year olds
# 16	60-64 year olds
# 17	65-69 year olds
# 18	70-74 year olds
# 19	75-79 year olds
# 20	80-84 year olds
# 21	85 years old and older
# 
# 
# The key for race/sex code is as follows:
#   
#   1 = White males
# 2 = White females
# 3 = Black males
# 4 = Black females
# 5 = Other race males
# 6 = Other race females

# read in data
file <- url('http://www.census.gov/popest/data/counties/asrh/pre-1980/tables/co-asr-7079.csv')
col_types <- 'iciiiiiiiiiiiiiiiiiii'
a <- seq(0,85,5)
b <- seq(4,89,5)
divisions <- paste0('age',a,'_',b)
col_names <- c('year', 'fipsCode', 'raceCode', divisions)
Census1970 <- readr::read_csv(file, col_names=FALSE, col_types=col_types)
names(Census1970) <- col_names

# creating race_gender factor
levelNames <- c('White Male', 'White Female', 'Black Male', 'Black Female', 'Other Race Male', 'Other Race Female')
Census1970$race_gender <- factor(Census1970$raceCode, levels=1:6, labels=levelNames)

# -----Creating state and county name factors------


# Create stateISO variable for Census1970
rownames(countyFips)<- as.character(countyFips$fips)
Census1970$stateISO <- countyFips[Census1970$fipsCode, 'stateISO']

# ----- Creating simple dataset of total population by county by year ---------

# CountyTotals1970s dataframe that contains total county population organized in county~year fashion
# which we will then seperate into unique years and attach to spatial polygons (??) 



totPop <- vector('numeric', nrow(Census1970)/6)
countyFipsCode <- vector('character', nrow(Census1970)/6)
year <- vector('numeric', nrow(Census1970)/6)

totPopIndex <- 1
for (i in seq(1, nrow(Census1970), 6)) {
  totPop[totPopIndex] <- sum(Census1970[i:(i+5), 4:21])
  countyFipsCode[totPopIndex] <- Census1970$fipsCode[i]
  year[totPopIndex] <- Census1970$year[i]
  totPopIndex <- totPopIndex + 1 
  
}

# NOTE: When reference a data frame, you get a dataframe. When referencing a 
# character vector, you get a character vector. A data frame is a list of vectors, 
# so even when using matrix syntax, we got a list of 390 1 by 1 dataframes, which 
# is why it must be Census1970$fipsCode (which is a character vector) 

df <- data.frame(year, totPop, countyFipsCode)
melted <- melt(df, id.vars=c('year', 'countyFipsCode'), measure.vars = 'totPop')

CountyTotals1970s <- dcast(melted, countyFipsCode~year)
CountyTotals1970s$CountyFips3 <- str_sub(CountyTotals1970s$countyFipsCode, start=-3) 
CountyTotals1970s$StateFips <- str_sub(CountyTotals1970s$countyFipsCode, end=-4)

WAOnly1970s <- subset(CountyTotals1970s, StateFips=='53') 

# As above, assign fips names as row names, sample baseed on that and it should return a re-ordered
# data frame.

#rownames(CountyTotals1970s)<- as.character(CountyTotals1970s$countyFipsCode)





# ----- Formatting to plot with MazamaSpatialUtils-----------------------------

# Census data (Census1970 and Census1970) have two character ISO codes and five 
# character county fips codes. sp dataset USCensusCounties has 3 character 
# county fips codes and state ISO codes. 
# We are rearranging the CountyTotals1970s (which data comes from df we made out of data
# from Census1970) based on the organization of the three digit county codes of 
# USCensusCounties so that we can plot maps. 

# Psuedocode: create 3 digit codes for df, create new reshaped df with 3 digit 
# codes, sort by referencing reshaped df with rownames of 3 digit codes by 
# USCensusCounties$countyFIPS

#We will do this in a seperate file





    