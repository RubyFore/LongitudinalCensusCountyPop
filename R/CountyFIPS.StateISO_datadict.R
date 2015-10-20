# ----- Step 2: Create a dictionary mapping state level ISO:FIPS ------------

# from Maps package
data(county.fips)

# Clean up funky names
county.fips$polyname <- str_replace(county.fips$polyname, ':.*$', '')
nameMatrix <- str_split_fixed(county.fips$polyname, ',', 2)
nameMatrix <- as.data.frame(nameMatrix)
# Add clean names into new CountyFips dataset
countyFips <- cbind(county.fips, nameMatrix)
countyFips <- countyFips[,c(1,3,4)]
countyFips <- dplyr::distinct(countyFips, fips)
names(countyFips) <- c('fips', 'stateName', 'countyName')
# Create sepearated county and state Codes 
countyFips$countyCode <- str_sub(countyFips$fips, -3)
countyFips$stateCode <- str_sub(countyFips$fips, end=-4)

# Add missing '0' to 4-digit county FIPS codes 
singleMask <- as.integer(countyFips$stateCode) < 10  
countyFips[singleMask,'stateCode'] <- paste0('0', countyFips[singleMask, 'stateCode'])
countyFips[singleMask,'fips'] <- paste0('0', countyFips[singleMask, 'fips'])

# Add state ISO code - requires another dataset (from Wikipedia) that correlates 
# state FIPs with state ISO code. 
stateISOtoFips <- read.csv('/Users/rubyfore/Downloads/StateISOtoFips.csv',stringsAsFactors=FALSE)
stateISOtoFips$Numeric.code[c(1,2,4,5,6,8,9,10,11)] <- paste0('0',stateISOtoFips$Numeric.code[c(1,2,4,5,6,8,9,10,11)])

# Adding state ISO to countyFips by adding rownames
rownames(stateISOtoFips) <- as.character(stateISOtoFips$Numeric.code)
countyFips$stateISO <- stateISOtoFips[countyFips$stateCode,'Alpha.code']

# CountyFips has the following columns:  "fips,stateName,countyName,countyCode,stateCode,stateISO"  
rownames(countyFips)<- as.character(countyFips$fips)

# TODO:  Why not just save this as a .RData file and then load when needed.
# TODO:  This should be pulled out as a separate function to be run only once EVER.

save(countyFips, file="countyFIPS.StateISO_datadict.Rdata")
