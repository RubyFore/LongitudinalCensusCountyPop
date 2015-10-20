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
stateISOtoFips <- read.csv('/Users/rubyfore/Downloads/StateISOtoFips.csv',stringsAsFactors=FALSE)
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




#why didn't this work? 
#if (length(countyFips$stateCode) < 2) {
# countyFips$stateCode <- paste0('0', countyFips$stateCode)
#}
# Add more columns ...



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
df1970 <- readr::read_csv(file, col_names=FALSE, col_types=col_types)
names(df1970) <- col_names

# creating race_gender factor
levelNames <- c('White Male', 'White Female', 'Black Male', 'Black Female', 'Other Race Male', 'Other Race Female')
df1970$race_gender <- factor(df1970$raceCode, levels=1:6, labels=levelNames)

# -----Creating state and county name factors------


# Create stateISO variable for df1970
rownames(countyFips)<- as.character(countyFips$fips)
df1970$stateISO <- countyFips[df1970$fipsCode, 'stateISO']

# create new total population variable
df1970$totpop <- rowSums(df1970[, 4:21])
#df1970simplenames(df1970simple) <- c('year', 'fips', 'race', 'totpop')
#<- df1970[, c(1:3, 22)]

# trying to get total county population by year. 

# Just WA dataset 
WA1970Census <- filter(df1970, stateISO=='WA')

#doesnt work
#WA1970Census$totPopByRace <- rowSums(WA1970Census[,4:21])

# a <- seq(1, nrow(WA1970Census), 6)
# b <- seq(6, nrow(WA1970Census), 6)
# totPop <- vector('numeric', nrow(WA1970Census)/6)
# totPop[1] <- sum(WA1970Census[1:6, 4:21])
# 
# for (i in 1:nrow(WA1970Census)/6) {
#   totPop[i] <- sum(WA1970Census[a[i]:b[i], 4:21])
# }


# ----- Creating simple dataset of total population by county by year ---------

# reshapedDF dataframe that contains total county population organized in county~year fashion
# which we will then seperate into unique years and attach to spatial polygons (??) 



totPop <- vector('numeric', nrow(WA1970Census)/6)
countyFipsCode <- vector('character', nrow(WA1970Census)/6)
year <- vector('numeric', nrow(WA1970Census)/6)

totPopIndex <- 1
for (i in seq(1, nrow(WA1970Census), 6)) {
  totPop[totPopIndex] <- sum(WA1970Census[i:(i+5), 4:21])
  countyFipsCode[totPopIndex] <- WA1970Census$fipsCode[i]
  year[totPopIndex] <- WA1970Census$year[i]
  totPopIndex <- totPopIndex + 1 
  
}

# NOTE: When reference a data frame, you get a dataframe. When referencing a 
# character vector, you get a character vector. A data frame is a list of vectors, 
# so even when using matrix syntax, we got a list of 390 1 by 1 dataframes, which 
# is why it must be WA1970Census$fipsCode (which is a character vector) 

df <- cbind(year, totPop, countyFipsCode)
df <- as.data.frame(df)
melted <- melt(df, id.vars=c('year', 'countyFipsCode'), measure.vars = 'totPop', factorsAsStrings=FALSE)

reshapedDf <- dcast(melted, countyFipsCode~year)

# As above, assign fips names as row names, sample baseed on that and it should return a re-ordered
# data frame.

rownames(reshapedDf)<- as.character(reshapedDf$countyFipsCode)





# ----- Formatting to plot with MazamaSpatialUtils-----------------------------

# Census data (df1970 and WA1970Census) have two character ISO codes and five 
# character county fips codes. sp dataset USCensusCounties has 3 character 
# county fips codes and state ISO codes. 
# We are rearranging the reshapedDf (which data comes from df we made out of data
# from WA1970Census) based on the organization of the three digit county codes of 
# USCensusCounties so that we can plot maps. 

# Psuedocode: create 3 digit codes for df, create new reshaped df with 3 digit 
# codes, sort by referencing reshaped df with rownames of 3 digit codes by 
# USCensusCounties$countyFIPS
df$countyFips3 <- str_sub(df$countyFipsCode, start=-3)
melted <- melt(df, id.vars=c('year', 'countyFips3'), measure.vars='totPop', factorsAsStrings=FALSE)

reshaped3digit <- dcast(melted, countyFips3~year)
rownames(reshaped3digit) <- reshaped3digit$countyFips3

# For just WA
load('/Users/rubyfore/Data/Spatial/USCensusCounties.RData')
WAdata <- subset(USCensusCounties, USCensusCounties$stateCode=='WA')
spatialPlotDf <- reshaped3digit[WAdata$countyFIPS,]

for (i in 2:11) {
  spatialPlotDf[[i]] <- as.numeric(as.character(spatialPlotDf[[i]]))
}



# PLOTTING !!!!!!!!!!!!

plot(WAdata)
WAdata@data <- cbind(WAdata@data, spatialPlotDf)

library(RColorBrewer)


# should be a function where you can enter the year and get default colored by 
# quantiles, or specify your breaks and number of breaks. 
colors <- brewer.pal(4,'YlOrRd')
breaks <- quantile(WAdata@data[[11]], na.rm=TRUE)
colorIndices <- .bincode(WAdata@data[[11]], breaks = breaks)
plotColors <- colors[colorIndices]
plot(WAdata, col=plotColors)





    