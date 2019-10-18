### functions file


# API call ----------------------------------------------------------------

#' @title footprintGetter
#' @description This function queries the goclimateneutral API and gets the values for one specific trip
#' @param from Origin airport code
#' @param to Destination airport code
#' @param connections are there any connections? provide as comma seperated list if more than one , Default: NULL
#' @param class what class the flight is in. Can be 'economy', 'premium_economy',
#' 'business' and 'first'., Default: 'economy'
#' @param API API Key. Contact goclimateneutral in order to obtain your own at https://www.goclimateneutral.org/contact
#' @return rturns a tibble with the origin and destination airport codes as well
#' as the tons of CO2 of that flight and the cost to offset, currently in SEK.
#' @details All flights are assumed to be 'to' and 'from' only, no connections (even though
#' this is an underestimation). It would be easy to
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname footprintGetter
#' @export
#' @importFrom httr GET authenticate content
#' @importFrom tibble tibble
#' @importFrom dplyr %>% bind_rows arrange mutate
#' @importFrom tidyr drop_na
#' @importFrom stringr str_split
footprintGetter <- function(from, to, connections = NULL, class = "economy", API){
  # browser()
  ## First parse connections
  if (!is.null(connections)) {
    ## There ARE connections, see how many and construct a df of from-to for all
    connections %>% str_split(",") %>% unlist -> splitConnections
    c(from, splitConnections, to) -> splitConnections
    tibble(num = 1:length(splitConnections),a = splitConnections) -> connectionsDF
    bind_rows(head(connectionsDF,nrow(connectionsDF) - 1),
              connectionsDF %>% arrange(desc(num))) %>%
      mutate(b = lead(a), num = 0:(2*nrow(connectionsDF) - 2)) %>% drop_na() -> connectionsDF
    paste0("segments[",connectionsDF$num,"][origin]=", connectionsDF$a,
           "&segments[",connectionsDF$num, "][destination]=", connectionsDF$b) %>%
      paste0(collapse = "&") -> flights
  } else {
    paste0("segments[0][origin]=", from, "segments[0][destination]=", to,
           "segments[1][origin]=", to, "segments[1][destination]=", from) %>%
      paste0(collapse = "&") -> flights
  }
  url_with_parameters <- paste0("https://api.goclimateneutral.org/v1/flight_footprint?", flights,"&cabin_class=", class, "&currencies[]=EUR")

  # "https://api.goclimateneutral.org/v1/flight_footprint?segments[0][origin]=BOS&segments[0][destination]=BCN&segments[1][origin]=BCN&segments[1][destination]=BOS&cabin_class=economy&currencies[]=EUR" ->url_with_parameters
  result <- httr::GET(url_with_parameters,
                      httr::authenticate(API,"", type = "basic")
  )
  content <- httr::content(result)

  ## error catcher
  if(length(content) != 3) return(tibble::tibble(from = NA, to = NA, footprint = NA, cost_sek = NA))

  ## just get the footprint & cost
  content <- tibble::tibble(
    from = from,
    to = to,
    footprint = content$footprint,
    cost_sek = content$offset_prices[[1]][[1]]/100)
  return(content)
}


# Network chart -----------------------------------------------------------

#' @title chartMaker
#' @description Creates a network great arcs map showing all the trips people have taken to get to the destination
#' @param lat_destiantion latitude of destination
#' @param lon_destination longitude of destination
#' @param lat_origin latitude of origin
#' @param lon_origin longitude of origin
#' @param color what color would you like the lines to
#' be (in hex code, like #efefef), Default: "f2f2f2"
#' @param lwd desired line thickness, Default:0.8
#' @return returns a map with great arcs drawn to it's destination
#' @details DETAILS
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @seealso
#'  \code{\link[maps]{map}}
#'  \code{\link[geosphere]{gcIntermediate}}
#' @rdname chartMaker
#' @export
#' @importFrom maps map
#' @importFrom geosphere gcIntermediate
chartMaker <- function(lat_destiantion, lon_destination, lat_origin,
                       lon_origin, color = "f2f2f2", lwd = 0.8){
  #xlim <- c(-171.738281, 180)
  #ylim <- c(12.039321, 71.856229)
  #maps::map("world", col="#f2f2f2", fill=TRUE, bg="white", lwd=0.05, xlim=xlim, ylim=ylim)
  maps::map("world")
  # browser()
  for (i in 1:length(lat_origin)) {
    inter <- geosphere::gcIntermediate(c(lon_destination[i], lat_destiantion[i]), c(lon_origin[i], lat_origin[i]), n=100, addStartEnd=TRUE)
    lines(inter, col= color, lwd = lwd)
  }
}


# coordinateGetter --------------------------------------------------------

#' @title coordinateGetter
#' @description takes an address and returns the coordinates of that point
#' @param address physical address. Works best in this format: 13 Elm Street, Amityville, State, Country. It's most important to get the two last points correct, so either City, Country, or State, Country (since that will give us decent granularity to find the nearest airport)
#' @return returns a data frame of the address, its latitude and its longitude
#' @details since we are using the free api with geocode, the matching is approximate, but good enough for our purposes.
#' @examples
#' \dontrun{
#' if(interactive()){
#'  library(purrr);library(magrittr)
#'  addresses <- c("9th Street, Troy, NY, USA", "The Mall, Solan, Himachal Pradesh, India")
#'  purrr::map_dfr(addresses, ~coordinateGetter(.))
#'
#'  }
#' }
#' @seealso
#'  \code{\link[ggmap]{geocode}}
#' @rdname coordinateGetter
#' @export
#' @importFrom ggmap geocode
#' @importFrom dplyr select bind_cols
coordinateGetter <- function(address){
  # browser()
  ## get addresses
  Locs <- ggmap::geocode(address, source = "dsk")
  ## combine addresses and coordinates
  returnDF <- bind_cols(address = address,
         location = Locs) %>% select(address, lat, lon)

  # ## sometimes it doesn't get them all, so fix that but only 3 times
  # for (i in 1:3){
  #   if(any(is.na(returnDF$location))){
  #     smallGeo <- returnDF %>% filter(is.na(location)) %>% pull %>% geocode
  #     smallDF <- tibble(address = returnDF %>% filter(is.na(location)) %>% pull(address),
  #                       location = smallGeo)
  #     returnDF <- left_join(returnDF, smallDF, by = "address")
  #   }
  # }
  return(returnDF)
}


# airport matcher ------------------------------------------------------

#' @title airportMatcher
#' @description finds the nearest airport to a set of coordinates
#' @param returnDF output of coordinateGetter
#' @param dataSet airport database do you want to use: "openFlights" or "the other one", default = "openFlights
#' @return a dataframe containing the address, the coordinates of the address, the IATA code of the nearest airport, the distance (in km) to the nearest airport, and the coordinates of the nearest airport
#' @details It's a bit tough to know what airports to use, it seems all corpuses are either too detailed or not enough. For now I'm using openflights, might parametrize this later to allow for other airport lists to be selected
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #'  library(purrr)
#'  addresses <- c("9th Street, Troy, NY, USA", "The Mall, Solan, Himachal Pradesh, India")
#'  returnDF <- purrr::map_dfr(addresses, ~coordinateGetter(.))
#'  airportMatcher(returnDF)
#'  }
#' }
#' @seealso
#'  \code{\link[geosphere]{distm}}
#' @rdname airportMatcher
#' @importFrom geosphere distm
#' @import purrr
#' @importFrom dplyr select left_join right_join filter
#' @importFrom utils read.csv
#' @importFrom stats na.exclude
#'
#' @export
airportMatcher <- function(returnDF, dataSet = "openFlights"){
  # browser()
  if (!"airports" %in% ls())  {
    if (dataSet == "openFlights") {
      read.csv("https://raw.githubusercontent.com/jpatokal/openflights/master/data/airports.dat") %>%
        select(5, 7, 8) %>% set_names(c("code", "lat_airport", "lon_airport")) %>%
        na.exclude() %>% filter(!grepl(pattern = "\\\\", code)) -> airports
    } else{
      read.csv("data/airport-codes_csv.csv")  -> airports
      airports %>%
        select(iata_code, coordinates) %>%
        separate(coordinates, c("1","2"), sep = ", ") %>% as_tibble %>%
        set_names(c("code", "lon_airport", "lat_airport")) %>%
        select(code,lat_airport,lon_airport) %>%
        na.exclude() %>% filter(code != "") %>% filter(code == "JFK") -> airports
    }
    }

  returnDF %>% na.exclude() -> returnDF_small
  geosphere::distm(returnDF_small %>% select(lon,lat) %>% as.matrix,
                     airports %>% select(lon_airport,lat_airport) %>%
                     as.matrix, fun = geosphere::distHaversine) -> distances
  airports$code -> colnames(distances)
  returnDF_small$address -> row.names(distances)

  array_branch(distances, 1) %>% purrr::map(~which.min(.x)) %>% map_chr(names) -> nearestAirport
  array_branch(distances, 1) %>% purrr::map_dbl(~min(.x)) -> nearestDistance
  returnDF_small %>% mutate(airport = nearestAirport %>% unname,
                      dist_km = nearestDistance/1000 %>% unname) -> returnDF_small

  returnDF_small %>% left_join(airports %>%
                           modify_if(is.factor, as.character), by = c("airport" = "code")) -> returnDF_small

  returnDF_small %>% select(-lat, -lon) %>% right_join(returnDF, by = "address") -> returnDF2
  return(returnDF2)
}


# exchangerateGetter ------------------------------------------------------

#' @title exchangerateGetter
#' @description get today's exchange rate SEK to USD

#' @param desiredcurrency what currency would we like to convert to, Default: USD
#' @return returns the numeric number of SEKs to the desired currency
#'
#' @details The API gives SEK by default, we want the output to do USD instead
#' @examples
#' \dontrun{
#' if(interactive()){
#'  #EXAMPLE1
#'  }
#' }
#' @rdname exchangerateGetter
#' @export
#' @importFrom httr GET content
exchangerateGetter <- function(desiredcurrency = "USD"){
  exchangerate <- httr::GET(url = paste0("https://api.exchangeratesapi.io/latest?symbols=",
                                         desiredcurrency,",SEK"))
  exchangerate <- httr::content(exchangerate)
  exchangerate <- unlist(exchangerate)
  return(as.numeric(exchangerate[1])/as.numeric(exchangerate[2]))
}
