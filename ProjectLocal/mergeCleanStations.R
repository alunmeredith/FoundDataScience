library(rmongodb)
library(dplyr)

### 
### FIRST DOWNLOAD THE EXISTING STATIONS DATASET FROM MONGODB
###

#------ Defining gloabl variables -----------
DB <- "DSproject"
CRIME <- paste(DB, "londonCrime", sep=".")
STATIONS <- paste(DB, "stations", sep=".")


#----- Creating mongo connection ------------
createMongo <- function() {
  if(exists("mongo")) try(mongo.destroy(mongo))
  mongo <<- mongo.create(host = "mongodb-dse662l3.cloudapp.net:27017", db = DB)
  return(mongo.is.connected(mongo))
}

#create mongo interface
createMongo()

#----- Cleaning mongoDB stations dataset ----

# Download stations as a dataframe to reduce querying
ifelse(mongo.is.connected(mongo), 
       stations <- mongo.find.all(mongo, STATIONS, data.frame=TRUE),
       {
         createMongo()
         ifelse(mongo.is.connected(mongo),
                stations <- mongo.find.all(mongo, STATIONS, data.frame=TRUE),
                stop("Could not connect to mongoDB"))
       })
# uploaded data is stored in strings so change the type of variables to be appropriate
stations$Latitude <- as.numeric(stations$Latitude)
stations$Longitude <- as.numeric(stations$Longitude)
stations$OS.X <- as.numeric(stations$OS.X)
stations$OS.X <- as.numeric(stations$OS.X)
stations$Zone <- as.factor(stations$Zone)
stations$Station <- as.character(stations$Station)

# Fix some discrepencies in station names
stations$Station <- gsub("^Acton$", "Acton Main Line", stations$Station)
stations$Station <- gsub("^Bank$", "Bank & Monument", stations$Station)
stations$Station <- gsub(" and ", " & ", stations$Station, perl = T)
stations$Station <- gsub("^Earls Court$", "Earl's Court", stations$Station)
stations$Station <- gsub("Bakerloo", "Bak", stations$Station)
stations$Station <- gsub("Circle/District/Hammersmith & City", "Cir", stations$Station)
stations$Station <- gsub("District", "Dis", stations$Station)
stations$Station <- gsub("Met.", "H&C", stations$Station)
stations$Station <- gsub("1 2 3", "123", stations$Station)
stations$Station <- gsub("^St Pancras$", "St.Pancras", stations$Station)




#----- Download/Cleaning trainUsage ----
# Usage statistic for these stations is 2014-2015 entry + exit counts. 
# Metadata for trainUsage can be found in the xls spreadsheet
trainUsage <- read.csv("station-usage.csv", stringsAsFactors = F)
# Select relevent observations and variables
trainUsage <- filter(trainUsage, Government.Office.Region..GOR. == "London")
trainUsage <- select(trainUsage, Station.Name, District.or.Unitary.Authority, Station.Facility.Owner, X1415.Entries...Exits, Explanation.of.large.change)
names(trainUsage) <- c("Station", "Borough", "Owner", "TicketUsage", "Comments")
trainUsage$TicketUsage <- as.numeric(gsub(",","", trainUsage$TicketUsage))

#----- Download/Cleaning tubeUsage
# Metadata for tubeUsage can be found in the xls spreadsheet
tubeUsage <- read.csv("multi-year-station-entry-and-exit-figures.csv", skip=6, stringsAsFactors = F, strip.white = T)
tubeUsage <- select(tubeUsage, Station, million, Note)
names(tubeUsage) <- c("Station", "EntryExitEst", "Comments")
# Remove some NA rows and "Total" row
tubeUsage <- filter(tubeUsage, !(Station == "Total"), !(Station == ""))
# Convert usage to units (instead of millions)
tubeUsage$EntryExitEst <- tubeUsage$EntryExitEst * 1000000
# Change the key of comments into their actual value
tubeUsage$Comments <- gsub("^A$", "", tubeUsage$Comments)
tubeUsage$Comments <- gsub("^B$", "Figures for Bank/Monument now exclude W&C line passengers interchanging with other lines", tubeUsage$Comments)
tubeUsage$Comments <- gsub("^T$", "Figures include all passengers on the W&C line (which is closed on Sundays", tubeUsage$Comments)
tubeUsage$Comments <- gsub("^N$", "Station Closed", tubeUsage$Comments)

#----- Merging trainUsage & Station ----
mergedStations <- merge(stations, trainUsage, "Station", all.x = TRUE)
mergedStations <- (merge(mergedStations, tubeUsage, "Station", all.x = TRUE))
mergedStations <- mutate(mergedStations, usage = ifelse(!is.na(TicketUsage), TicketUsage, EntryExitEst))

