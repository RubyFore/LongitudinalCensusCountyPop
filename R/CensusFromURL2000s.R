
load('Data/CountyPopulation/CountyFIPS.StateISO_datadict.RData')


# ----- Reading and tidying data for 2000s by county --------------------------
file2000s <- url('http://www.census.gov/popest/data/intercensal/county/files/CO-EST00INT-TOT.csv')
CountyTotals2000s <- readr::read_csv(file2000s, col_names=TRUE)
CountyTotals2000s[[5]] <- as.character(CountyTotals2000s[[5]])

CountyTotals2000s[[5]] <- paste0('00', CountyTotals2000s[[5]])
CountyTotals2000s[[5]] <- str_sub(CountyTotals2000s[[5]], start=-3)
CountyTotals2000s[[4]] <- paste0('0', CountyTotals2000s[[4]])
CountyTotals2000s[[4]] <- str_sub(CountyTotals2000s[[4]], start=-2)

names(CountyTotals2000s) <- c('SUMLEV', 'Region', 'Division', 'StateFips', 'CountyFips3','StateName', 
                              'CountyNames', 'baseEstimate2000', 'y2000', 'y2001', 'y2002', 'y2003', 'y2004', 
                              'y2005', 'y2006', 'y2007', 'y2008', '2009', 'Census2010', 'y2010')

CountyTotals2000s$countyFipsCode <- paste0(CountyTotals2000s$StateFips, CountyTotals2000s$CountyFips3)

rownames(CountyTotals2000s) <- as.character(CountyTotals2000s$countyFipsCode)

CountyTotals2000s <- subset(CountyTotals2000s, CountyFips3 != '000')



WAOnly2000s <- subset(CountyTotals2000s, StateFips=="53")
# NOTE: WAOnly2000s returned class as a 'tbl_df' and 'data.frame', which I was not intending. 
# NOTE: So I coerced it into just a dataframe, and then the values were in spatialPlot2000s
WAOnly2000s <- as.data.frame(WAOnly2000s)
rownames(WAOnly2000s) <- as.character(WAOnly2000s$COUNTY)

WAOnly2000s <- WAOnly2000s[2:40, 4:20]

load('/Users/rubyfore/Data/Spatial/USCensusCounties.RData')
WAdata <- subset(USCensusCounties, USCensusCounties$stateCode=='WA')
spatialPlot2000s <-WAOnly2000s[WAdata$countyFIPS,] 


