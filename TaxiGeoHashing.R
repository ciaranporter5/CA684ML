# Import the unique latitude/longitude values and perform geohashing
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

write.csv(as.data.frame(table(TaxiPickups$GeohashP3)),file = "PickUp-Geo3.csv")
write.csv(as.data.frame(table(TaxiPickups$GeohashP4)),file = "PickUp-Geo4.csv")
write.csv(as.data.frame(table(TaxiPickups$GeohashP5)),file = "PickUp-Geo5.csv")
write.csv(as.data.frame(table(TaxiPickups$GeohashP6)),file = "PickUp-Geo6.csv")

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

# summary retained geohashes at precision level 6
GeoHashP6Summary <- as.data.frame(count(
  TaxiPickups, c("GeohashP6", "retain")))
GeoHashP6Summary <- GeoHashP6Summary[GeoHashP6Summary$retain==1,]
write.csv(GeoHashP6Summary,file = "TaxiPickupsP6Summary.csv")

# summarise
TaxiPickupsRetain <- TaxiPickups[TaxiPickups$retain==1,]
TaxiPickupsRetain <- TaxiPickupsRetain[, c(1, 4, 5, 6,7)]
write.csv(TaxiPickupsRetain,file = "TaxiPickupsRetain.csv")

# plot co-ordinates of boundaries 
leaflet(data = map_decoded) %>% addProviderTiles('Esri') %>%
  addMarkers(~lng, ~lat, popup = map_decoded$geohash)

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






