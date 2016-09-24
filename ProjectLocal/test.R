## Source mongo connections, libraries and distance functions
source('rHeader.R')

# CrimeDensities function calculates a list of the different measures of crime densities for a subset of crimes
# If the subset is a crimetype then this will be factored into the borough crime density value. 
crimeDensities <- function(crimes, station, crimeType) { 
  if (length(crimes) == 0) { return(
    list(crimesInCircle = 0, crimeDensity = 0, crimeDensityGaussian = 0,
         crimeDensityPerUsage = 0, crimeDensityPerUsageGaussian = 0,
         crimeDensityVsBorough = 0, crimeDensityVsBoroughGaussian = 0))
  }
  crimesCircle <- length(crimes)
  crimeDensity <- crimesCircle / area
  # Calculate Gaussian Density
  gaussWeights <- sapply(crimes, function(x) { 
    dnorm(x[["StationDistance"]])
  })
  crimeDensityG <- sum(gaussWeights) / area
  
  # Usage normalised
  crimeDensityPerUsage <- crimeDensity / station[["usage"]]
  crimeDensityPerUsageG <- crimeDensityG / station[["usage"]]
  # Borough normalised
  createMongo()
  query <- sprintf( '{ "LocalAuthority": "%s" }', station[["Borough"]])
  boroughCrimeDensity <- mongo.count(mongo, CRIME, query) / boroughSizes[boroughSizes$boroughs == station[["Borough"]], "AreaKm"]
  crimeDensityVsBorough <- crimeDensity / boroughCrimeDensity
  crimeDensityVsBoroughG <- crimeDensityG / boroughCrimeDensity
  
  return(list(crimesInCircle = crimesCircle, crimeDensity = crimeDensity, crimeDensityGaussian = crimeDensityG,
              crimeDensityPerUsage = crimeDensityPerUsage, crimeDensityPerUsageGaussian = crimeDensityPerUsageG,
              crimeDensityVsBorough = crimeDensityVsBorough, crimeDensityVsBoroughGaussian = crimeDensityVsBoroughG))
}   

createMongo()
# Download borough area sizes
boroughSizes <- mongo.find.all(mongo, BOROUGHS, data.frame = TRUE)
# Download list of stations
stations <- mongo.find.all(mongo, STATIONSCRIME, '{ "Count": { "$exists": false }}')

# Calculate area of circle
MAX_DIST <- 0.3
AREA <- pi * (MAX_DIST/2)^2
SIG_NORM <- 0.2



# loop through each station
sapply(1:length(stations), function(index){
  area <- AREA
  station <- stations[[index]]
  
  ##### Calculate Area ----
  # Check if there is another station overlapping the cirlce
  #     if (station[["nearestDist"]] < MAX_DIST*2) {
  #         stations[index, "Overlapping"] <<- TRUE
  #         d <- station["nearestDist"]
  #         r <- MAX_DIST
  #         overlap <- 2 * r^2 * acos(d/(2*r)) - (d/2) * sqrt(4*r^2 - d^2)
  #         area <<- AREA - overlap/2
  #         }
  
  ##### Calculate total crime densities ----
  ## query all the crimes for the station inside the circle 
  t0 <- Sys.time()
  print( paste("station:", stations[[index]]["Station"]))
         createMongo()
         query <- sprintf('{ "Station": "%s", "StationDistance": { "$lte": %f }}', station[["Station"]], MAX_DIST)
         crimes <- mongo.find.all(mongo, CRIME, query)
         t1 <- Sys.time()
         print( "time to query crimes within distance:" )
         print( t1 - t0 )
         
         
         totals <-  c(crimeType = "Totals", crimeDensities(crimes,station))
         
         crimeTypes <- mongo.distinct(mongo, CRIME, "Crimetype")
         
         densities <- sapply(crimeTypes, function(crimetype) {
           t00 <- Sys.time()
           print( crimetype )
           query <- sprintf('{ "Station": "%s", "StationDistance": { "$lte": %f }, "Crimetype": "%s"}',
                            station[["Station"]], MAX_DIST, crimetype)
           crimes <- mongo.find.all(mongo, CRIME, query)
           t01 <- Sys.time()
           print( "Time to find crimes within distance" )
           print ( t01 - t00)
           row <- c(crimetype, crimeDensities(crimes, station, crimetype))
           t-2 <- Sys.time()
           print( "Time to calculate crime densities" )
           print ( t02 - t01 )
         })
         densities <- t(cbind(totals, densities))
         
         ###  update queries
         createMongo()
         query <- sprintf( '{ "Station": "%s" }', station[["Station"]] )
         update <- list( "$set" = list( "Count" = densities[,2], "Crime Density" = densities[,3], "Crime Density (Gaussian)" = densities[,4], "Crime Density Per Usage" = densities[,5], "Crime Density Per Usage (Gaussian)" = densities[,6], "Crime Density Vs Borough" = densities[,7], "Crime Density Vs Borough (Gaussian)" = densities[,8]))
         mongo.update(mongo, STATIONSCRIME, query, update)
         
         print(paste(station[["Station"]], Sys.time()))
})