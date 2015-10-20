# ----- Create Longitudinal Dataset for Washington from existing decades ------

# must source these four datasets: CensusFromURL1970s, CensusFromURL1980s, 
# CensusFromURL1990s, CensusFromURL2000s. All have 39 rows and rownames of 
# 3 digit county FIPS codes. 

source('~/Projects/CensusFromURL1970.R', echo=TRUE)
source('~/Projects/CensusFromURL1980.R', echo=TRUE)
source('~/Projects/CensusFromURL1990s.R', echo=TRUE)
source('~/Projects/CensusFromURL2000s.R', echo=TRUE)


# ----- Creating Longitudinal dataset for WA ----------------------------------


# * WAOnly1970s has 11 variables, countyFips3 then 10 rows of data named 1970, 1971, etc. 
# * WAOnly1980s has 13 variables, countyFipsCode (5 digit), 10 rows of data, 
# named 1980, 1982, etc, then StateFIPS and countyFips3
# * WAOnly1990s has 13 variables, FIPSCode, 10 rows of data named 1990, 1991, etc. 
# StateCode, CountyFIPSCode
# * WAOnly2000s has 17 variables, "STATE"             "COUNTY"            "STNAME"            "CTYNAME"           
# "ESTIMATESBASE2000" "POPESTIMATE2000"   "POPESTIMATE2001"   "POPESTIMATE2002"   "POPESTIMATE2003"   
# "POPESTIMATE2004"  "POPESTIMATE2005"   "POPESTIMATE2006"   "POPESTIMATE2007"   "POPESTIMATE2008"     
# "POPESTIMATE2009" "CENSUS2010POP"     "POPESTIMATE2010"

# We Want WAOnly1970s[,,] + WAOnly1980s[,2:11] + WAOnly1990s[,2:11] + WAOnly2000s[,6:16]

WATotals <- cbind(WAOnly1970s[,12], WAOnly2000s[,4], WAOnly1970s[,2:11], WAOnly1980s[,2:11], WAOnly1990s[,2:11], WAOnly2000s[,6:16])
names(WATotals)[33:43] <- c('2000', '2001', '2002', '2003', '2004', '2005', '2006', '2007', '2008', '2009', '2010')

# PROBLEM: first 10 columns are characters, 2nd 10 are numeric. Rest are integers,
# PROBLEM: we want all to be integers. 

save(WATotals, file='/Users/rubyfore/Data/CountyPopulation/WATotalPopByCounty2.RData')

# For just WA
load('/Users/rubyfore/Data/Spatial/USCensusCounties.RData')


# data is called USCensusCounties
load('/Users/rubyfore/Data/CountyPopulation/WATotalPopByCounty2.RData')
colnames(WATotals)[[1]] <- 'CountyFips'
colnames(WATotals)[[2]] <- 'CountyName'
rownames(WATotals) <- WATotals$CountyFips
# data set is called WATotals
WAdata <- subset(USCensusCounties, USCensusCounties$stateCode=='WA')

spatialPlotDf <- WATotals[WAdata$countyFIPS,]

temp <- data.frame(WAdata, spatialPlotDf)
WAdata@data <- temp




# PLOTTING !!!!!!!!!!!!

plot(WAdata)

library(RColorBrewer)


# should be a function where you can enter the year and get default colored by 
# quantiles, or specify your breaks and number of breaks. 
colors <- brewer.pal(4,'YlOrRd')
breaks <- quantile(WAdata@data[,30], na.rm=TRUE)
colorIndices <- .bincode(WAdata@data[[20]], breaks = breaks)
plotColors <- colors[colorIndices]


plot(WAdata, col=plotColors)


for (i in 22:ncol(WAdata@data)){
  colors <- brewer.pal(8, 'YlOrRd')
  breaks <- quantile(WAdata@data[,52], probs = seq(0, 1, 0.125), na.rm=TRUE)
  colorIndices <- .bincode(WAdata@data[[i]], breaks = breaks)
  plotColors <- colors[colorIndices] 
  plot(WAdata, col=plotColors)
  title(str_sub(names(WAdata@data)[[i]], start = 2)) 
}



plot(WAdata, col=plotColors)

# -----Creating Longitudinal Dataset for the Entire US ------------------------

USCountyPopulationTotals <- data.frame(CountyTotals1970s[,c(1,13,12)], CountyTotals1970s[,2:11], 
                                       CountyTotals1980s[,2:11], CountyTotals1990s[,2:11])


finalCountyTotPop <- merge(USCountyPopulationTotals, CountyTotals2000s, by.x='countyFipsCode', by.y='countyFipsCode', all=TRUE)
finalCountyTotPop <- finalCountyTotPop[,c(1:33, 42:51, 53)]


save(finalCountyTotPop, file='/Users/rubyfore/Data/CountyPopulation/USTotalPopByCounty.RData')


# ----- Plotting Texas --------------------------------------------------------
load('/Users/rubyfore/Data/CountyPopulation/USTotalPopByCounty.RData')
load('/Users/rubyfore/Data/Spatial/USCensusCounties.RData')


TXtotals <- subset(finalCountyTotPop, StateFIPS == '48')
TXspdf <- subset(USCensusCounties, USCensusCounties$stateCode=='TX')

rownames(TXtotals) <- TXtotals$CountyFIPS3
TXtotalsReordered <- TXtotals[TXspdf$countyFIPS,]
TXtemp <- data.frame(TXspdf@data, TXtotalsReordered) 

TXspdf@data <- TXtemp
  
library(RColorBrewer)
library(stringr)

for (i in 22:ncol(TXspdf@data)){
  colors <- brewer.pal(8, 'YlOrRd')
  breaks <- quantile(TXspdf@data[,52], probs = seq(0, 1, 0.125), na.rm=TRUE)
  colorIndices <- .bincode(TXspdf@data[[i]], breaks = breaks)
  plotColors <- colors[colorIndices] 
  plot(TXspdf, col=plotColors)
  title(str_sub(names(TXspdf@data)[[i]], start = 2)) 
}
  
