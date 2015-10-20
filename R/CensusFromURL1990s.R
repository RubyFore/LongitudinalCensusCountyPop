# ----- Reading in and Reshaping data from the 1990s ------------------------------------
library(dplyr)
library(stringr)

col_names <- c('year', 'FIPSCode', 'age', 'race_gender', 'ethnicity', 'pop')
#NOTE: grouby_by doesn't work on objects of class 'character', but changing it to an integer
# NOTE: makes it drop a zero. Whats the solution? write a for loop that adds two zeroes and
# NOTE: then cuts off? or just don't care about the rest and cut off the ones that are 
# NOTE: FIPS code starting with 53? 
col_types <- c('integer', 'character', 'integer', 'integer', 'integer', 'integer')
df1990 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1990.txt',
                         col.names=col_names, colClasses=col_types)
df1991 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1991.txt',
                            col.names=col_names, colClasses=col_types)
df1992 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1992.txt',
                            col.names=col_names, colClasses=col_types)
df1993 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1993.txt',
                            col.names=col_names, colClasses=col_types)
df1994 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1994.txt',
                            col.names=col_names, colClasses=col_types)
df1995 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1995.txt',
                            col.names=col_names, colClasses=col_types)
df1996 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1996.txt',
                            col.names=col_names, colClasses=col_types)
df1997 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1997.txt',
                            col.names=col_names, colClasses=col_types)
df1998 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1998.txt',
                            col.names=col_names, colClasses=col_types)
df1999 <- read.table('http://www.census.gov/popest/data/intercensal/st-co/tables/STCH-Intercensal/STCH-icen1999.txt',
                            col.names=col_names, colClasses=col_types)
#NOTE/TODO: Could create a function that accepts the year, and does the following things:
# * assemble the URL
# * download the data using readr::read_table
# * dplyr stuff/function
# * return dataframe

Create1990sTotals <- function(df){
  df %>%
    group_by(FIPSCode) %>%
    summarize(totalPop=sum(pop, na.rm=TRUE))  %>%
    arrange(FIPSCode) -> FIPStotals
  return(FIPStotals)
}

totals1990 <- Create1990sTotals(df1990)
totals1991 <- Create1990sTotals(df1991)
totals1992 <- Create1990sTotals(df1992)
totals1993 <- Create1990sTotals(df1993)
totals1994 <- Create1990sTotals(df1994)
totals1995 <- Create1990sTotals(df1995)         
totals1996 <- Create1990sTotals(df1996)
totals1997 <- Create1990sTotals(df1997)
totals1998 <- Create1990sTotals(df1998)
totals1999 <- Create1990sTotals(df1999)

CountyTotals1990s <- cbind(totals1990, totals1991[[2]], totals1992[[2]], totals1993[[2]], totals1994[[2]], totals1995[[2]], 
                     totals1996[[2]], totals1997[[2]], totals1998[[2]], totals1999[[2]])
names(CountyTotals1990s) <- c('FIPSCode', '1990', '1991', '1992', '1993', '1994', '1995', '1996', '1997', '1998', '1999')

CountyTotals1990s$FIPSCode <- paste0('0', CountyTotals1990s$FIPSCode)
CountyTotals1990s$FIPSCode <- str_sub(CountyTotals1990s$FIPSCode, -5)
CountyTotals1990s$StateCode <- str_sub(CountyTotals1990s$FIPSCode, end=2)
CountyTotals1990s$CountyFIPSCode <- str_sub(CountyTotals1990s$FIPSCode, start=-3)



WAOnly1990s <- subset(CountyTotals1990s, CountyTotals1990s$StateCode=='53')
rownames(WAOnly1990s) <- as.character(WAOnly1990s$CountyFIPSCode)


#dfList <- c(df1990, df1991, df1992, df1993, df1994, df1995, df1996, df1997, df1998, df1999)
#totalsList <- c() 
#for (i in 1:10) {
 # dfList[[i]] %>%
  #group_by(FIPSCode) %>%
   # summarize(totalPop=sum(pop, na.rm=TRUE)) -> totalsList[[i]]
#}

# QUESTION: When you find yourself retyping things, do you want to write a function 
# or a for loop? I think I actually want to write a function for above, not a for loop
