#' Calculate great circle distance between two airports. Distances are calculated using the
#' Haversine formula which assumes a spherical earth. Distances are returned in kilometres
#'
#' @param airport1 Takes a three-letter IATA code corresponding to an airport
#' @param airport2 as above
#' @return Great circle distance in kilometres between the two airports
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

