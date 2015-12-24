# library(rmongodb)
# 
# #------ Defining gloabl variables ---------
# DB <- "DSproject"
# CRIME <- paste(DB, "londonCrime", sep=".")
# STATIONS <- paste(DB, "stations", sep=".")
# 
# 
# #------ Defining functions ----------------
# 
# # Creating mongo connection
# createMongo <- function() {
#   if(exists("mongo")) try(mongo.destroy(mongo))
#   mongo <<- mongo.create(host = "mongodb-dse662l3.cloudapp.net:27017", db = DB)
#   return(mongo.is.connected(mongo))
# }
# 
# # Calculate distance between two points
# distance <- function(lat1, long1, lat2, long2) {
#   
#   # Convert arguments to radians
#   lat1 = pi * lat1 / 180
#   lat2 = pi * lat2 / 180
#   long1 = pi * long1 / 180
#   long2 = pi * long2 / 180
#   
#   # Calculates distance using
#   a <- sin( (lat1-lat2)/2 )^2 + cos(lat1)*cos(lat2)* sin( (long1-long2)/2 )^2
#   c = 2 * atan2( sqrt(a), sqrt(1-a))
#   d = 6371 * c
#   return(d)
# }

#------ Start of Script --------------

#create mongo interface
createMongo()

# Download stations as a dataframe to reduce querying
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

begin <- Sys.time()
# Create cursor for all crimes not already indexed
ifelse(mongo.is.connected(mongo),
       crimeCursor <- mongo.find( mongo, CRIME, '{ "station" : { "$exists" : false } }'),
       ifelse(createMongo(), 
              crimeCursor <- mongo.find( mongo, CRIME, '{ "station" : { "$exists" : false } }'),
              stop("Couldn't connect to mongo server")))


i = 0L
while(mongo.cursor.next(crimeCursor)) {
  print( paste("Documents processed: ", i,sep = ""))
  # Generate list of distances to each station (km)
  crime <- mongo.bson.to.list(mongo.cursor.value(crimeCursor))
  
  # Some crime have no geolocation data
  if( (crime$Latitude == "") | (crime$Longitude == "")) {
    print(mongo.cursor.value(crimeCursor["_id"]))
    mongo.remove(mongo, CRIME, list("_id" = crime$`_id`))
    print(paste("No value for Lat/Long: removed", mongo.oid.to.string(mongo.bson.to.list(mongo.cursor.value(crimeCursor))[["_id"]]), "from collection", sep=" "))
    next
  }
  
  distances <- sapply( 1:nrow(stations), function(x) {
    distance( crime$Latitude, crime$Longitude, stations[x, "Latitude"] , stations[x, "Longitude"])
  })
  
  # update document in mongodb
  minDist <- min(distances)
  station <- stations$Station[which.min(distances)]
  crime$station <- station
  crime$stationDist <- minDist
  updated <- mongo.bson.from.list(crime)
  mongo.update(mongo, CRIME, list("_id" = crime$`_id`), crime)
  
  # Some transport crimes not in london so remove from mongo if found
  if( min(distances) > 100) {
    mongo.remove(mongo, CRIME, list("_id" = crime$`_id`))
    print(paste("Location (", mongo.bson.to.list(mongo.cursor.value(crimeCursor))["Location"], ") Not in London: removed ", mongo.oid.to.string(mongo.bson.to.list(mongo.cursor.value(crimeCursor))[["_id"]]), " from collection", sep = ""))
    next
  }
  
  i = i+1
}

print(Sys.time() - begin)

#unprocessed <- mongo.count( mongo, CRIME, '{ "station" : { "$exists" : false } }')
#processed <- mongo.count( mongo, CRIME, '{ "station" : { "$exists" : true } }')

#print(paste( 100*processed/(processed+unprocessed), "% complete, last run took", time, "seconds", sep = " "))
