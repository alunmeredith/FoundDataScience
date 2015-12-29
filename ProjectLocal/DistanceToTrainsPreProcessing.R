### README
#   - Processes all city of london, met and british transport police csvs (directory
#   Method is specific so you may have to change this to work it on your pc).
#   - No longer does filtering methods like removing the non-london btp entries, this
#   should be quite easy to achieve afterwards on mongodb by filtering distance values.
#   - For the sake of using a Date type all the month data state they are on the 1st day
#   of the month, this is just for the sake of using operators on it later, the day 
#   should otherwise be ignored.

### Possible changes to make
#   - Make it write to csv instead could be faster, could improve stability, because
#   we know which files have been processed successfully and don't have to start,
#   from the beginning. But could also be slower, harder to achieve and loose the 
#   date type. 
#   - Make it save the files it has processed and not process them again, by searching
#   mongodb for specific entries. 
#   - Make it process in smaller batches to reduce the effect / chance of errors. 
#   - Make it process to a list in the apply function to reduce conversions
#   - Do the draw a circle around stations thing for large files


library(rmongodb)
library(dplyr)

t0 <- Sys.time()

#------ Defining gloabl variables ---------
DB <- "DSproject"
CRIME <- paste(DB, "test", sep = ".")
STATIONS <- paste(DB, "stations", sep = ".")
startTime <- Sys.time()
mongo <- NA
totalProcessed <- 0
filesProcessed <- 0

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

t1 <- Sys.time()
print(paste("Time to initialise :", round(t1 - t0, 4), attr(t1 - t0, "units")))

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
stations$Station <- as.character(stations$Station)

LSOA <- read.csv("OA11_LSOA11_MSOA11_LAD11_EW_LUv2.csv", stringsAsFactors = F)
names(LSOA) <- c("OutputAreaCode", "LSOAcode", "LSOAname", "MSOAcode", "MSOAname", "LAcode", "LAname", "LAWelshName")

t2 <- Sys.time()
print(paste("Time to download trains / read LSOA data :", round(t2 - t1, 4), attr(t2 - t1, "units")))

### Preprocessing -------------
# Generate array of files to process
files <- list.files(path = "./../CrimeDV/DataProject/Datasets/NationalData", full.names = TRUE, recursive = TRUE)
fileNames <- list.files(path = "./../CrimeDV/DataProject/Datasets/NationalData", full.names = FALSE, recursive = TRUE)
londonfiles <- grep( pattern = ("city-of-london-street|metropolitan-street|btp-street"), files, value = TRUE)
londonNames <- grep( pattern = ("city-of-london-street|metropolitan-street|btp-street"), fileNames, value = TRUE)
londonNames <- gsub("^.*?/", "", londonNames)

#######################
#for (i in (filesProcessed + 1):length(londonfiles)) {
for (i in seq(3, 90, 3)) {
    t2a <- Sys.time()
    table <- read.csv(londonfiles[i])
    entries <- nrow(table)
    
    t3 <- Sys.time()
    print(paste("Processing:", londonfiles[i]))
    print(paste("Time to read csv:", round(t3 - t2a, 4), attr(t3 - t2a, "units"), 
                 "(", entries, " Entries )"))
    
    # Loop over every station for every crime in file
    processed <- apply(table, 1, function(x){
        # Calculate distance to station  
        distances <- apply(stations, 1, function(y) {
            distRad(as.numeric(x["Latitude"]), as.numeric(x["Longitude"]),
                     as.numeric(y["Latitude"]), as.numeric(y["Longitude"]))    
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
        
        # Calculate LSOA name and LA name
        x["LocalAuthority"] <- filter(LSOA, LSOAcode == x["LSOA.code"])[1,"LAname"]
        return(x)
    })
    
    t4 <- Sys.time()
    print(paste("Time to make calculations:", round(t4 - t3, 4), attr(t4 - t3, "units")))
    
    dataframe <- as.data.frame(t(processed), stringsAsFactors = F)
    dataframe$Latitude <- as.numeric(dataframe$Latitude)
    dataframe$Longitude <- as.numeric(dataframe$Longitude)
    dataframe$StationDistance <- as.numeric(dataframe$StationDistance)
    dataframe$ISOMonth <- as.POSIXlt(paste0(dataframe$Month, "-01"), "%Y-%m-%d", tz = "GMT")
    bson <- mongo.bson.from.df(dataframe)
    # Clean the names by removing the '.' (e.g. "LSOA.code")
    names(dataframe) <- gsub("\\.", "", names(dataframe))

    
    t5 <- Sys.time()
    print(paste("Time to process bson:", round(t5 - t4, 4), attr(t5 - t4, "units")))
    
    write.csv(dataframe, paste0("./processed/", londonNames[i]))
    # Insert into mongodb (must be done in batches of ~40000 or fails)
    createMongo()
    batchSize <- 1000
    batches <- ceiling( entries / batchSize )
    remainder <- entries %% batchSize
    for (j in 1:(batches)) {
        if (batches != 1) {
            if ( j == batches) {
                min <- entries - remainder
                createMongo
                mongo.insert.batch(mongo, CRIME, bson[min:entries])
            } else {
               min <- ((j - 1)*batchSize + 1)
               max <- j*(batchSize)
               createMongo()
               mongo.insert.batch(mongo, CRIME, bson[ min:max ])
            }

        } else {
            createMongo()
            mongo.insert.batch(mongo, CRIME, bson)
        }
        print(paste("Batch", j, "of", batches, "inserted"))
    }
    t6 <- Sys.time()
    print(paste("Time to insert documents:", round(t6 - t5, 4), attr(t6 - t5, "units")))
    totalProcessed <- totalProcessed + entries
    print(paste("Total Entries Processed:", totalProcessed))
}   
    print(Sys.time() - startTime)
