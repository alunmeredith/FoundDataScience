library(rmongodb)
library(dplyr)

#------ Defining gloabl variables ---------
DB <- "DSproject"
CRIME <- paste(DB, "londonCrime3", sep = ".")
STATIONS <- paste(DB, "stations", sep = ".")
startTime <- Sys.time()
mongo <- NA

# Creating mongo connection
createMongo <- function() {
    if (exists("mongo")) {
        try(mongo.destroy(mongo))
    }
    mongo <<- mongo.create(host = "mongodb-dse662l3.cloudapp.net:27017", db = DB)
    return(mongo.is.connected(mongo))
}

# Downloading Datasets
createMongo()
stations <- mongo.find.all(mongo, STATIONS)
boroughCrime <- read.csv("1_LondonBoroughWiseCrimeData.csv", stringsAsFactors = FALSE)

invisible(lapply(stations[[1]], function(x) {
    # Find all crimes associated with that station
    stationCrime <- mongo.find.all(mongo, CRIME, (paste0('{ "Station":"', x[["Station"]] ,'"}')))
    # Calculate a weighted crime density
    #lapply(stationCrime, )
}))

