

## Documentation:https://api.goclimateneutral.org/docs

library(XML)
library(bitops)
library(RCurl)
latlon2ft <- function(origin,destination){
  xml.url <- paste0('http://maps.googleapis.com/maps/api/distancematrix/xml?origins=',origin,'&destinations=',destination,'&mode=driving&sensor=false')
  xmlfile <- xmlParse(getURL(xml.url))
  dist <- xmlValue(xmlChildren(xpathApply(xmlfile,"//distance")[[1]])$value)
  distance <- as.numeric(sub(" km","",dist))
  ft <- distance*3.28084 # FROM METER TO FEET
  return(ft)
}







# Network chart -----------------------------------------------------------

## Airports from https://datahub.io/core/airport-codes meh.
## https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat
## https://blog.exploratory.io/calculating-distances-between-two-geo-coded-locations-358e65fcafae

airports <- read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat")

# or nycflights13::airports

## other approaches
get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
  loadNamespace("purrr")
  loadNamespace("geosphere")
  longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
  longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
  distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
  distance_m = list_extract(distance_list, position = 1)
  if (units == "km") {
    distance = distance_m / 1000.0;
  }
  else if (units == "miles") {
    distance = distance_m / 1609.344
  }
  else {
    distance = distance_m
    # This will return in meter as same way as distHaversine function.
  }
  distance
}


## or library(geosphere)
# geosphere::distm(c(lon1, lat1), c(lon2, lat2), fun = distHaversine)
## From: http://www.nagraj.net/notes/calculating-geographic-distance-with-r/
## ggmap::mapdist()


# Formal start ------------------------------------------------------------

## get my data:

library(tidyverse)
library(googlesheets4)

gs <- read_sheet("Guest list and Wedding budget")
df <- gs_read(ss = gs, ws = 1)

locations <- df %>% filter(!is.na(`Mailing address`)) %>% pull(`Mailing address`)

## get the lat/lon for our people:
returnDF <- coordinateGetter(locations)

## and now map to nearest airports
withAirports <- airportMatcher(returnDF)

## and now plot those vs London
df1 <- returnDF %>% select(lon, lat) %>% na.exclude(.)

chartMaker(lat1 = rep(51.1638229, nrow(df1)),
           lon1 = rep(0.1312621, nrow(df1)),
           lat2 = df1$lat,
           lon2 = df1$lon,
           color = "purple",
           lwd = 0.8)

## and get the footprint
# footprintGetter(from = "FNL", to = "LHR", class = "economy", API = "d29a6f1481b5365611fb708d")

withAirports$airport %>% map(~safely(footprintGetter)(from = .x, to = "LHR",
                             class = "economy", API = "d29a6f1481b5365611fb708d")) -> footprints
footprints %>% map("error")

footprints %>% map_dfr("result") %>% summarize(cost = sum(cost_sek))

allFlights <- footprints %>% map_dfr("result") %>%
  select(from, to) %>% unite(both, from, to, sep = "%2C") %>%
  bind_cols(footprints %>% map_dfr("result") %>%
              select(from, to) %>% unite(both, to, from, sep = "%2C")) %>%
  unlist %>% paste(collapse = "%2C")

paste0("https://www.goclimateneutral.org/flight_offsets/new?offset_params=economy%2C",
       allFlights, '&locale="en"') %>% cat

  https://api.exchangeratesapi.io/latest?symbols=USD,GBP
