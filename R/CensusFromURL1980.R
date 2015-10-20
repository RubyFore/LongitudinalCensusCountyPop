
# ----- Reading and tidying data for 1980s by county --------------------------
file1980 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1980.csv')
file1981 <-('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1981.csv')
file1982 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1982.csv')
file1983 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1983.csv')
file1984 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1984.csv')
file1985 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1985.csv')
file1986 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1986.csv')
file1987 <-('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1987.csv')
file1988 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1988.csv')
file1989 <- ('https://www.census.gov/popest/data/counties/asrh/1980s/tables/PE-02-1989.csv')

col_types <- 'icciiiiiiiiiiiiiiiiii'
a <- seq(0,85,5)
b <- seq(4,89,5)
divisions <- paste0('age',a,'_',b)
col_names <- c('year', 'fipsCode', 'raceCode', divisions)

df1980 <- readr::read_csv(file1980, col_names=col_names, skip=7, col_types=col_types)
df1981 <- readr::read_csv(file1981, col_names=col_names, skip=7, col_types=col_types)
df1982 <- readr::read_csv(file1982, col_names=col_names, skip=7, col_types=col_types)
df1983 <- readr::read_csv(file1983, col_names=col_names, skip=7, col_types=col_types)
df1984 <- readr::read_csv(file1984, col_names=col_names, skip=7, col_types=col_types)
df1985 <- readr::read_csv(file1985, col_names=col_names, skip=7, col_types=col_types)
df1986 <- readr::read_csv(file1986, col_names=col_names, skip=7, col_types=col_types)
df1987 <- readr::read_csv(file1987, col_names=col_names, skip=7, col_types=col_types)
df1988 <- readr::read_csv(file1988, col_names=col_names, skip=7, col_types=col_types)
df1989 <- readr::read_csv(file1989, col_names=col_names, skip=7, col_types=col_types)

df1980s <- rbind(df1980, df1981, df1982, df1983, df1984, df1985, df1986, df1987, df1988, df1989)
names(df1980s) <- col_names

totPop <- vector('numeric', nrow(df1980s)/6)
countyFipsCode <- vector('character', nrow(df1980s)/6)
year <- vector('numeric', nrow(df1980s)/6)

index <- 1
for (i in seq(1, nrow(df1980s), 6)) {
  totPop[index] <- sum(df1980s[i:(i+5), 4:21])
  countyFipsCode[index] <- df1980s$fipsCode[i]
  year[index] <- df1980s$year[i]
  index <- index + 1   
}


CountyTotals1980s_long <- data.frame(year, totPop, countyFipsCode)
CountyTotals1980s_long$countyFips3 <- str_sub(CountyTotals1980s_long$countyFipsCode, start=-3)
melted <- melt(CountyTotals1980s_long, id.vars=c('year', 'countyFipsCode'), measure.vars = 'totPop', factorsAsStrings=FALSE)

CountyTotals1980s <- dcast(melted, countyFipsCode~year)
CountyTotals1980s$StateFIPS <- str_sub(CountyTotals1980s$countyFipsCode, end=-4)
CountyTotals1980s$countyFips3<- str_sub(CountyTotals1980s$countyFipsCode, start=-3)


WAOnly1980s <- subset(CountyTotals1980s, StateFIPS=='53')
rownames(WAOnly1980s) <- as.character(WAOnly1980s$countyFips3)



