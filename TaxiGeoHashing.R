# Import the unique latitude/longitude values and perform geohashing
# install.packages("geohash")
# install.packages("leaflet")

library(geohash)
library(leaflet)
library(plyr)

# read in csv data showing pickup latitudes and longitudes
TaxiPickups <- read.csv("Raw Data/distinct_latlong_outputs_clean.csv")

# set geohashes at varying levels of precision
TaxiPickups$GeohashP3 <- gh_encode(lats = TaxiPickups$Taxi.Lat, 
                                   lngs = TaxiPickups$Taxi.Long, precision = 3)
TaxiPickups$GeohashP4 <- gh_encode(lats = TaxiPickups$Taxi.Lat, 
                                   lngs = TaxiPickups$Taxi.Long, precision = 4)
TaxiPickups$GeohashP5 <- gh_encode(lats = TaxiPickups$Taxi.Lat, 
                                   lngs = TaxiPickups$Taxi.Long, precision = 5)
TaxiPickups$GeohashP6 <- gh_encode(lats = TaxiPickups$Taxi.Lat, 
                                   lngs = TaxiPickups$Taxi.Long, precision = 6)

# count the number of occurances of unique geohashes
length(unique(TaxiPickups$GeohashP3))
length(unique(TaxiPickups$GeohashP4))
length(unique(TaxiPickups$GeohashP5))
length(unique(TaxiPickups$GeohashP6))

# set to generic lat/long for ploting
taxi_decodedP3 <- gh_decode(unique(TaxiPickups$GeohashP3))
taxi_decodedP4 <- gh_decode(unique(TaxiPickups$GeohashP4))
taxi_decodedP5 <- gh_decode(unique(TaxiPickups$GeohashP5))
taxi_decodedP6 <- gh_decode(unique(TaxiPickups$GeohashP6))

# boundary ranges
map_sample <- c('dr73', 'dr79', 'dr5q', 'dr5w')
map_decoded <- gh_decode(map_sample)
map_decoded$geohash <- map_sample

# list neighbours
gh_neighbours('dr73') # top left
gh_neighbours('dr79') # top right
gh_neighbours('dr5q') # bottom left
gh_neighbours('dr5w') # bottom right

# remove entries outside boundary ranges
TaxiPickups$retain<-
  ifelse(
  ((TaxiPickups$Taxi.Lat <=  gh_decode('dr73')$lat) &
  (TaxiPickups$Taxi.Lat >=  gh_decode('dr5w')$lat) &
  (TaxiPickups$Taxi.Long >=  gh_decode('dr73')$lng) &
  (TaxiPickups$Taxi.Long <=  gh_decode('dr5w')$lng)),1,0)

# summarise pickups and output
TaxiPickupsRetain <- TaxiPickups[TaxiPickups$retain==1,]
TaxiPickupsRetain <- TaxiPickupsRetain[, c(2, 3, 4, 5, 6, 7)]
write.csv(TaxiPickupsRetain,file = "TaxiPickupsRetain.csv", row.names = FALSE)

# plot co-ordinates of boundaries 
leaflet(data = map_decoded) %>% addProviderTiles('Esri') %>%
  addMarkers(~lng, ~lat, popup = map_decoded$geohash)

leaflet() %>% addTiles() %>%
  addRectangles(
    lng1=gh_decode('dr73')$lng, lat1=gh_decode('dr73')$lat,
    lng2=gh_decode('dr5w')$lng, lat2=gh_decode('dr5w')$lat,
    fillColor = "transparent"
  )

# plot on map using leaflet library
leaflet(data = taxi_decodedP3) %>% addTiles() %>%
  addMarkers(~lng, ~lat, 
             popup = paste(as.character(taxi_decodedP3$lat), 
                           as.character(taxi_decodedP3$lng), sep = "-"))

leaflet(data = taxi_decodedP4) %>% addTiles() %>%
  addMarkers(~lng, ~lat, 
             popup = paste(as.character(taxi_decodedP4$lat), 
                           as.character(taxi_decodedP4$lng), sep = "-"))

leaflet(data = taxi_decodedP5) %>% addTiles() %>%
  addMarkers(~lng, ~lat, 
             popup = paste(as.character(taxi_decodedP5$lat), 
                           as.character(taxi_decodedP5$lng), sep = "-"))

leaflet(data = taxi_decodedP6) %>% addTiles() %>%
  addMarkers(~lng, ~lat, 
             popup = paste(as.character(taxi_decodedP6$lat), 
                           as.character(taxi_decodedP6$lng), sep = "-"))

# read-in drop off data
TaxiDropoffs <- read.csv("Raw Data/DropOffSummary.csv")

# set geohashes at varying levels of precision
TaxiDropoffs$GeohashP3 <- gh_encode(lats = TaxiDropoffs$Dropoff_Latitude, 
                                   lngs = TaxiDropoffs$Dropoff_Longitude
                                   , precision = 3)
TaxiDropoffs$GeohashP4 <- gh_encode(lats = TaxiDropoffs$Dropoff_Latitude, 
                                   lngs = TaxiDropoffs$Dropoff_Longitude
                                   , precision = 4)
TaxiDropoffs$GeohashP5 <- gh_encode(lats = TaxiDropoffs$Dropoff_Latitude, 
                                   lngs = TaxiDropoffs$Dropoff_Longitude
                                   , precision = 5)
TaxiDropoffs$GeohashP6 <- gh_encode(lats = TaxiDropoffs$Dropoff_Latitude, 
                                   lngs = TaxiDropoffs$Dropoff_Longitude
                                   , precision = 6)

# count the number of occurances of unique geohashes
length(unique(TaxiDropoffs$GeohashP3))
length(unique(TaxiDropoffs$GeohashP4))
length(unique(TaxiDropoffs$GeohashP5))
length(unique(TaxiDropoffs$GeohashP6))

# remove entries outside boundary ranges
TaxiDropoffs$retain<-
  ifelse(
    ((TaxiDropoffs$Dropoff_Latitude <=  gh_decode('dr73')$lat) &
       (TaxiDropoffs$Dropoff_Latitude >=  gh_decode('dr5w')$lat) &
       (TaxiDropoffs$Dropoff_Longitude >=  gh_decode('dr73')$lng) &
       (TaxiDropoffs$Dropoff_Longitude <=  gh_decode('dr5w')$lng)),1,0)

# include only entries that need to be retained
TaxiDropoffsRetain <- TaxiDropoffs[TaxiDropoffs$retain==1,]
TaxiDropoffsRetain <- TaxiDropoffsRetain[, -7]
write.csv(TaxiDropoffsRetain,file = "TaxiDropOffsRetain.csv", row.names = FALSE)

# check removed entries
TaxiDropoffsRemove <- TaxiDropoffs[TaxiDropoffs$retain==0,]
TaxiDropoffsRemove <- TaxiDropoffsRemove[, -7]

length(unique(TaxiDropoffsRetain$GeohashP3))
length(unique(TaxiDropoffsRetain$GeohashP4))
length(unique(TaxiDropoffsRetain$GeohashP5))
length(unique(TaxiDropoffsRetain$GeohashP6))
