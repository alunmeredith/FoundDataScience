## Source mongo connections, libraries and distance functions
source('rHeader.R')

# Download list of stations
createMongo()
stations <- mongo.find.all(mongo, STATIONS)

# Calculate area of circle
MAX_DIST <- 0.3
AREA <- pi * (MAX_DIST/2)^2 

# loop through each station
sapply(1:length(stations), function(index){
    area <- AREA
    station <- stations[[index]]

    # Check if there is another station overlapping the cirlce
    if (station["nearestDist"] < MAX_DIST*2) {
        stations[index, "Overlapping"] <<- TRUE
        d <- station["nearestDist"]
        r <- MAX_DIST
        overlap <- 2 * r^2 * acos(d/(2*r)) - (d/2) * sqrt(4*r^2 - d^2)
        area <<- AREA - overlap/2
        }
    
    ## query all the crimes for the station inside the circle 
    createMongo()
    query <- sprintf('{ "Station": "%s", "StationDistance": { "$lte": %f }}', station[["Station"]], MAX_DIST)
    crimes <- mongo.find.all(mongo, CRIME, query)
    
    # Calculate and update density
    density <- length(crimes) / area
    createMongo()
    query <- sprintf( '{ "Station": %s }, "$set" { "CrimeDensity": %f, "Crimes": %d}', station[["Station"]], density, length(crimes))
    mongo.update(mongo, STATIONS, query)
    
    
    ## TODO: test this code
    ## TODO: update other crime summaries such as totoal crimes of different types.
    ## TODO: apply weighting functions such as gaussian
})