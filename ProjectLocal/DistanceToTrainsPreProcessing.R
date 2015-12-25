library(rmongodb)

#------ Defining gloabl variables ---------
DB <- "DSproject"
CRIME <- paste(DB, "test", sep=".")
STATIONS <- paste(DB, "stations", sep=".")
startTime <- Sys.time()

# Creating mongo connection
createMongo <- function() {
  if(exists("mongo")) try(mongo.destroy(mongo))
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
  a <- sin( (lat1-lat2)/2 )^2 + cos(lat1)*cos(lat2)* sin( (long1-long2)/2 )^2
  c = 2 * atan2( sqrt(a), sqrt(1-a))
  d = 6371 * c
  return(d)
}


## Download Trains Dataset ------------
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



### Preprocessing -------------
# Generate array of files to process
files <- list.files(path = "./../CrimeDV/DataProject/Datasets/NationalData", full.names = TRUE, recursive = TRUE)
londonfiles <- grep( pattern = ("city-of-london-street|metropolitan-street|tbp-street"), files, value = TRUE)


table <- read.csv(londonfiles[1])
# Loop over every station for every crime in file
test <- apply(table, 1, function(x){
  # Calculate distance to station  
  distances <- apply(stations, 1, function(y) {
    distance(as.numeric(x["Latitude"]), as.numeric(x["Longitude"]),
             as.numeric(y["Latitude"]), as.numeric(y["Longitude"]))    
    })
  
  # Adding station distance and name dimensions to crime vector
  x["StationDistance"] <- max(distances)
  x["Station"] <- stations$Station[which.max(distances)]

  # Check to ensure data is fit to be inserted into mongodb
  #if( !is.na(x["Station"]) & !is.na(x["StationDistance"]) & as.numeric(x["StationDistance"]) < 1.5) {
    # Insert vector x into mongodb
    bson <- mongo.bson.from.list(as.list(x))
    mongo.insert(mongo, CRIME, bson)
  #}
  })

Sys.time() - startTime