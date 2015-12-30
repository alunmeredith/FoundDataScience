#### In this script we process randomly generaged points in the same way as the crime points to try and get a better idea of how our data is differing from random. The random point generator used is http://www.geomidpoint.com/random/ and it is used by generating points in a 15mile circle around 51.507350, -0.12775829999998223. A smaller 15mile circle is used because we can't as easily relate the coordinates to london boroughs like we can with the LSOA codes in the crime data so we use a conservative estimate on the size of london to ensure most the data is in london boroughs. Even so this may not be the case but we can keep this in mind when looking at the tail of the histogram. 
library(dplyr)
library(rmongodb)
DB <- "DSproject"
RANDOM <- paste(DB, "randomPoints", sep = ".")
STATIONS <- paste(DB, "stations", sep = ".")
startTime <- Sys.time()

# Creating mongo connection
createMongo <- function() {
  if (exists("mongo")) {
    try(mongo.destroy(mongo))
  }
  mongo <<- mongo.create(host = "mongodb-dse662l3.cloudapp.net:27017", db = DB)
  return(mongo.is.connected(mongo))
}
createMongo()

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

#### READ DATQA

# Coordinate data
coords <- read.csv("randomCoordinatesCenter.csv", header=F)
coords <- coords %>% select(2, 4)
names(coords) <- c("Latitude", "Longitude")

# LSOA data
LSOA <- read.csv("OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv", stringsAsFactors = F)
names(LSOA) <- c("OutputAreaCode", "LSOAcode", "LSOAname", "MSOAcode", "MSOAname", "LAcode", "LAname", "LAWelshName")

# Station data
ifelse(mongo.is.connected(mongo), 
       lstations <- mongo.find.all(mongo, STATIONS),
       {
         createMongo()
         ifelse(mongo.is.connected(mongo),
                lstations <- mongo.find.all(mongo, STATIONS),
                stop("Could not connect to mongoDB"))
       })

stations <- data.frame(
  Station = sapply(lstations, function(x) x$Station),
  Latitude = sapply(lstations, function(x) x$Latitude),
  Longitude = sapply(lstations, function(x) x$Longitude))
stations$Latitude <- as.numeric(stations$Latitude)
stations$Longitude <- as.numeric(stations$Longitude)
stations$Station <- as.character(stations$Station)



### PROCESS -------------

# Loop over every station for every crime in file
processed <- apply(coords, 1, function(x){
  # Calculate distance to station  
  distances <- apply(stations, 1, function(y) {
    distRad(as.numeric(x["Latitude"]), as.numeric(x["Longitude"]), as.numeric(y["Latitude"]), as.numeric(y["Longitude"]))    
  })
  
  # Adding station distance and name dimensions to crime vector
  # Check to ensure data is fit to be inserted into mongodb
  if (all(is.na(distances))) {
    x["Station"] <- NA
    x["StationDistance"] <- NA
  }
  else {
    index <- which.min(distances)
    x["Station"] <- stations$Station[index]
    kilometers <- distance(as.numeric(x["Latitude"]), as.numeric(x["Longitude"]),
                           as.numeric(stations$Latitude[index]),
                           as.numeric(stations$Longitude[index]))
    x["StationDistance"] <- kilometers
  }
  return(x)
})


dataframe <- as.data.frame(t(processed), stringsAsFactors = F)
dataframe$Latitude <- as.numeric(dataframe$Latitude)
dataframe$Longitude <- as.numeric(dataframe$Longitude)
dataframe$StationDistance <- as.numeric(dataframe$StationDistance)

# Insert into mongodb
#bson <- mongo.bson.from.df(dataframe)
#write.csv(dataframe, "rgenCrimePoints.csv")
#createMongo()
#mongo.insert.batch(mongo, RANDOM, bson)
    