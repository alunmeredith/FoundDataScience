
####
library(ggmap)
library(mapproj)
library(rworldmap)
library(rworldxtra)
library(rmongodb)
source('rHeader.R')


############################################  MAPS WITH SCHOOLS ##############################

# download secSchoolsLond dataset and turn it into dataframe
SchoolsLoc <- mongo.find.all(mongo, SCHOOLS)
schoolsLocations <- do.call(rbind.data.frame, SchoolsLoc)
# delete first column (_id)
schoolsLocations <- schoolsLocations[,2:12]
################################################### All Schools Map ##########################
######### Create London map ################

map <- get_map(location = 'London', zoom = 10)
ggmap(map)
#newmap <- getMap(resolution = "high")
#plot(newmap, xlim = c(-3, 3), ylim = c(47, 53), asp = 2)

mapPoints <- ggmap(map) + geom_point(aes(x = Longitude, y = Latitude, size = 3), data = schoolsLocations , alpha = .5)
plot(mapPoints)


###                                  %%%%%%%% Should I add road M25 boundary??
################################################### FILTERED SCHOOLS MAPS:  GENDER ###########

######### Create London map

mapGender <- get_map(location = 'London', zoom = 10)
ggmap(mapGender)
#newmap <- getMap(resolution = "high")
#plot(newmap, xlim = c(-3, 3), ylim = c(47, 53), asp = 2)

schoolsLocationsGirls <- schoolsLocations[schoolsLocations$Gender=="Girls",]
schoolsLocationsBoys <- schoolsLocations[schoolsLocations$Gender=="Boys",]
schoolsLocationsMixed <- schoolsLocations[schoolsLocations$Gender=="Mixed",]


mapPointsGender <- ggmap(mapGender) + geom_point(aes(x = Longitude, y = Latitude, size = 3, color = "red"), data = schoolsLocationsGirls , alpha = .5) + 
  geom_point(aes(x = Longitude, y = Latitude, size = 3), data = schoolsLocationsBoys, color = "cadetblue4" , alpha = .5) +
  geom_point(aes(x = Longitude, y = Latitude, size = 3), data = schoolsLocationsMixed ,color = "black", alpha = .5)

##    %%%%%%%%%     scale_color/size...( ) to scale colors/size  --> put the related field to color inside aes() function in this case

### maybe if I set color = (crimeCount) is going to set the color in function of it, already scaled
             
plot(mapPointsGender)

# ============= Mongo query for station distance =============

createMongo()
query <- '{ "StationDistance": { "$lte": 0.3}}'
fields <- '{ "Station": 1, "_id":0, "StationDistance":1, "Crimetype":1}'
dataframe <- mongo.find.all(mongo, CRIME, query = query, fields = fields, data.frame = T)

# ============ Mongo query for stations crime ===============
mongo.findOne(mongo, STATIONS)

createMongo()
database <- mongo.find.all(mongo, STATIONS)

### initialise vectors ####
Station <- vector()
Latitude <- vector()
Longitude <- vector()
Density <- vector()
Count <- vector()
Usage <- vector()
DensityGaussian <- vector()
DensityUsage <- vector()
DensityBoroughs <- vector()
### apply function ####
invisible(sapply(database, function(x) {
    Station <<- c(Station, x$Station)
    Latitude <<- c(Latitude,x$Latitude)
    Longitude <<- c(Longitude, x$Longitude)
    ifelse(is.null(x$usage), tmp <- 0, tmp <- x$usage)
    Usage <<- c(Usage, tmp)
    ifelse(is.null(x$`Crime Density`$totals), 
           tmp <- 0, 
           tmp <- x$`Crime Density`$totals)
    Density <<- c(Density, tmp)
    
    ifelse(is.null( x$Count$totals), 
           tmp <- 0, 
           tmp <-  x$Count$totals)
    Count <<- c(Count, tmp)
    
    ifelse(is.null(x$`Crime Density (Gaussian)`$totals), 
           tmp <- 0, 
           x$`Crime Density (Gaussian)`$totals)
    DensityGaussian <<- c(DensityGaussian, tmp)
    
    ifelse(is.null( x$`Crime Density Per Usage`$totals), 
           tmp <- 0, 
           x$`Crime Density Per Usage`$totals)
    DensityUsage <<- c(DensityUsage, tmp)
    
    ifelse(is.null( x$`Crime Density Vs Borough`$totals), 
           tmp <- 0, 
           x$`Crime Density Vs Borough`$totals)
    DensityBoroughs <<- c(DensityBoroughs, tmp)
}))
dataframe1 <- as.data.frame(data.frame(Station = Station, Latitude = Latitude, Longitude = Longitude, Usage = Usage, Density = Density, Count = Count, DensityUsage = DensityUsage, DensityGaussian = DensityGaussian))
# ============ MAP
map <- get_map(location = 'London', zoom = 10)

# Map of Crime Density
gg <- ggmap(map) +
    geom_point(aes(x = Longitude, y = Latitude, colour = log(Density), size = Usage), data = dataframe1) +
    scale_color_continuous(low = "blue", high = "red") +
    scale_size_continuous(range = c(1,3)) +
    ggtitle("Crime Density")

gg <- ggmap(map) +
    geom_point(aes(x = Longitude, y = Latitude, colour = Count, size = Usage), data = dataframe1) +
    scale_color_continuous(low = "blue", high = "red") +
    scale_size_continuous(range = c(1,3)) +
    ggtitle("Crime Count")

gg <- ggmap(map) +
    geom_point(aes(x = Longitude, y = Latitude, colour = DensityGaussian, size = Usage), data = dataframe1) +
    scale_color_continuous(low = "blue", high = "red") +
    scale_size_continuous(range = c(2,4)) +
    ggtitle("Density Gaussian")

gg
