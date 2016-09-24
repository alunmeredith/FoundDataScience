source('rHeader.R')

createMongo()
boroughs <- as.data.frame(mongo.distinct(mongo, STATIONS, "Borough"))
boroughs <- boroughs[order(boroughs),]
mileAreas <- c(13.94,33.49,23.38,16.70,57.97,8.40, 1.12, 33.41,21.44,31.74,18.28,7.36,6.33,11.42,19.49,43.35,44.67,21.61,5.74,4.68,14.38,10.36,13.57,14.52,13.98,21.78,22.17,11.14,16.93,7.63,14.99,13.23,8.29)
boroughsDF <- data.frame( boroughs = boroughs, AreaMiles = mileAreas)
boroughsDF$AreaKm <- mileAreas * 2.58999

mongo.insert.batch(mongo, BOROUGHS, mongo.bson.from.df(boroughsDF))