
####

library(rworldmap)
library(rworldxtra)
library(rmongodb)
source('rHeader.R')


##################################### uploading new school mongodb dataset (dont need this for stations)

secSchoolsLond <- data.frame(
  School = secSchoolsLondon$EstablishmentName,
  Latitude = secSchoolsLondon$Latitude,
  Longitude = secSchoolsLondon$Longitude,
  Borough = secSchoolsLondon$LA..name.,
  TypeOfSchool = secSchoolsLondon$TypeOfEstablishment..name.,
  PhaseOfEducation = secSchoolsLondon$PhaseOfEducation..name.,
  Gender = secSchoolsLondon$Gender..name.,
  ReligiousCharacter = secSchoolsLondon$ReligiousCharacter..name.,
  Postcode = secSchoolsLondon$Postcode, stringsAsFactors = FALSE)

secSchoolsLond$Latitude <- as.numeric(secSchoolsLond$Latitude)
secSchoolsLond$Longitude <- as.numeric(secSchoolsLond$Longitude)
secSchoolsLond$School <- as.character(secSchoolsLond$School)
secSchoolsLond$Borough <- as.character(secSchoolsLond$Borough)
secSchoolsLond$TypeOfSchool <- as.character(secSchoolsLond$TypeOfSchool)
secSchoolsLond$PhaseOfEducation <- as.character(secSchoolsLond$PhaseOfEducation)
secSchoolsLond$Gender <- as.character(secSchoolsLond$Gender)
secSchoolsLond$ReligiousCharacter <- as.character(secSchoolsLond$ReligiousCharacter)
secSchoolsLond$Postcode <- as.character(secSchoolsLond$Postcode)

secSchoolsLond <- filter(secSchoolsLond, !grepl(c("^Bowden House School$|^Bradstow School$"), secSchoolsLond$School))

secSchoolsLondonUpdate <- cbind(secSchoolsLond, schoolsLocations[,4:5])
secSchoolsLondonUpdate$NearestSchool <- as.character(secSchoolsLondonUpdate$NearestSchool)
secSchoolsLondonUpdate$NearestSchoolDist <- as.numeric(secSchoolsLondonUpdate$NearestSchoolDist)

createMongo()
secSchoolsBson <-  mongo.bson.from.df(secSchoolsLondonUpdate)
mongo.insert.batch(mongo, SCHOOLS, secSchoolsBson)


####################################################################################################################################
############################################  MAPS WITH SCHOOLS ####################################################################
####################################################################################################################################

# download secSchoolsLond dataset and turn it into dataframe
SchoolsLoc <- mongo.find.all(mongo, SCHOOLS)
schoolsLocations <- do.call(rbind.data.frame, SchoolsLoc)
# delete first column (_id)
schoolsLocations <- schoolsLocations[,2:12]

####################################################################################################################################
################################################### All Schools Map ################################################################
####################################################################################################################################

library(ggmap)
library(mapproj)

######### Create London map

map <- get_map(location = 'London', zoom = 10)
ggmap(map)
#newmap <- getMap(resolution = "high")
#plot(newmap, xlim = c(-3, 3), ylim = c(47, 53), asp = 2)

mapPoints <- ggmap(map) + geom_point(aes(x = Longitude, y = Latitude, size = 3), data = schoolsLocations , alpha = .5)
plot(mapPoints)


###                                  %%%%%%%% Should I add road M25 boundary??

####################################################################################################################################
################################################### FILTERED SCHOOLS MAPS:  GENDER #################################################
####################################################################################################################################

library(ggmap)
library(mapproj)

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







