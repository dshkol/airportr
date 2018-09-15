#' Calculate great circle distance between two airports
#'
#' A function that calculates distances between pairs of airport codes. Distances are calculated using the Haversine formula which assumes a spherical earth. Distances are returned in kilometres.
#'
#' @param airport1 Takes a three-letter IATA code corresponding to an airport
#' @param airport2 As above
#' @return The great circle distance in kilometres between the two airports
#'
#' @export
#'
#' @examples
#' airport_distance("YVR","YYZ")
airport_distance <- function(airport1, airport2) {
  # Distance is calculated using the Haversine formula to calculate great circle
  # distances between points based off of the description detailed at
  # http://www.movable-type.co.uk/scripts/latlong.html
  #
  # The Haversine formula is reasonably accurate for great circle distances but does
  # not account for Earth's ellipsoidal nature, and can result in errors of around 0.3%.
  #
  # Use at your own risk and not for anything important.
  #
  # For a more robust implementation of distances between coordinates, consider the
  # Vincenty Ellipsoid methods, which are found in the geosphere package
  data("airports", envir=environment())
  match1 <- airports %>% dplyr::filter(IATA == airport1)
  match2 <- airports %>% dplyr::filter(IATA == airport2)
  if(sum(lengths(match1))==0 | sum(lengths(match2))==0) {
    stop("One or more supplied IATA codes are invalid")
  }
  lon1 = match1$Longitude * pi/180
  lat1 = match1$Latitude * pi/180
  lon2 = match2$Longitude * pi/180
  lat2 = match2$Latitude * pi/180

  radius = 6373

  dlon = lon2-lon1
  dlat = lat2-lat1

  a = (sin(dlat/2))^2 + cos(lat1) * cos(lat2) * (sin(dlon/2))^2
  b = 2 * atan2(sqrt(a), sqrt(1-a) )
  d = radius * b
  d
}

#' Lookup airports nearby other airports
#'
#' A function that returns details of airports within a user-specified distance of a given airport.
#'
#' @param input An airport name, IATA code, or ICAO code. Input type will be guessed unless
#' explicitly defined
#' @param distance Distance boundary for nearest airport lookup in kilometres
#' @return A tibble of airports that fall within the specified range of input airport
#'
#' @export
#'
#' @examples
#' airports_near_airport("YVR")
#'
#' # Or with a user specified distance in kilometres
#' airports_near_airport("YVR", distance = 200)
airports_near_airport <- function(input, distance = 100) {
  data("airports", envir=environment())
  match <- airportr::airport_detail(input)
  latrad <- match$Latitude * pi/180
  lat_distance = distance/110.574
  lon_distance = distance/(111.320*cos(latrad))
  matches <- airports %>% dplyr::filter(dplyr::between(Latitude, match$Latitude-lat_distance, match$Latitude+lat_distance),
                                        dplyr::between(Longitude, match$Longitude-lon_distance, match$Longitude+lon_distance))
  matches
}

#' Lookup airports near specified coordinates
#'
#' A function that returns details of all airports within a user-specified distance of an input coordinate location. Takes as input a longitude and latitude argument.
#'
#' @param lon Longitude in decimal degrees
#' @param lat Latitude in decimal degrees
#' @param distance Distance boundary for nearest airport lookup in kilometres
#' @return A tibble of airports that fall within the specified range of specified location
#'
#' @export
#'
#' @examples
#' airports_around(-123,49.2)
#'
#' # Or with a user specified distance in kilometres
#' airports_around(-123, 49.2, distance = 200)
airports_around <- function(lat, lon, distance = 100) {
  latrad <- lat * pi/180
  lat_distance = distance/110.574
  lon_distance = distance/(111.320*cos(latrad))
  data("airports", envir=environment())
  matches <- airports %>% dplyr::filter(dplyr::between(Latitude, lat-lat_distance, lat+lat_distance),
                                        dplyr::between(Longitude, lon-lon_distance, lon+lon_distance))
  matches
}
