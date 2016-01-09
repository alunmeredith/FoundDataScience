library(rmongodb)
library(dplyr)

#------ Defining gloabl variables ---------
DB <- "DSproject"
CRIME <- paste(DB, "londonCrime3", sep = ".")
STATIONS <- paste(DB, "stationsCrime3", sep = ".")
SCHOOLS <- paste(DB, "secSchoolsLond", sep = ".")
CRIMESCHOOLS <- paste(DB, "londonCrime6", sep = ".")
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

# Calculate distance between two points
distance <- function(lat1, long1, lat2, long2) {
    
    # Convert arguments to radians
    lat1 = pi * lat1 / 180
    lat2 = pi * lat2 / 180
    long1 = pi * long1 / 180
    long2 = pi * long2 / 180
    
    # Calculates distance using
    a <- sin( (lat1 - lat2)/2 ) ^ 2 + cos(lat1)*cos(lat2)* sin( (long1 - long2)/2 ) ^ 2
    c = 2 * atan2( sqrt(a), sqrt(1 - a))
    d = 6371 * c
    return(d)
}

distRad <- function(lat1, long1, lat2, long2) {
    a <- (lat1 - lat2) ^ 2 + (long2 - long1) ^ 2   
    return(a)
}