library(rmongodb)


# Creating mongo connection
mongo <- mongo.create(host = "mongodb-dse662l3.cloudapp.net:27017", db = "DSproject")
mongo.is.connected(mongo)


## Inserting tubedata to collection
tubeList <- list()
tubeList <- apply(tubes, 1, function(x) c(tubeList, x))
res <- lapply(tubeList, function(x) mongo.bson.from.list(x))

if(mongo.is.connected(mongo) == TRUE) {
  mongo.insert.batch(mongo, "DSproject.stations", res)
}


mongo.findOne( mongo, "DSproject.stations")
mongo.count( mongo, "DSproject.stations" )