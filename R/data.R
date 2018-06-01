#' Table of airport detail data
#'
#' A dataset containing names, codes, locations, altitude, and timezones for airports
#'
#' @format A data frame with 7184 rows and 14 variables:
#' \describe{
#'   \item{OpenFlights ID}{OpenFlights database ID}
#'   \item{Name}{Airport name, sometimes contains name of the city}
#'   \item{City}{Name of city served by airport}
#'   \item{Country}{Name of country where airport is located}
#'   \item{IATA}{3-letter IATA code}
#'   \item{ICAO}{4-letter ICAO code}
#'   \item{Latitude}{Latitude in decimal degrees}
#'   \item{Longitude}{Longitude in decimal degrees}
#'   \item{Altitude}{Altitude in feet}
#'   \item{UTC}{Hours offset from UTC}
#'   \item{DST}{Daylight savings time. One of E (Europe), A (US/Canada), S (South America), O
#'   (Australia), Z (New Zealand), N (None) or U (Unknown)}
#'   \item{Timezone}{Timezone in Olson format}
#'   \item{Type}{Type of airport}
#'   \item{Source}{Source of data. Airport data generally sourced from OurAirports}
#' }
#' @source \url{https://openflights.org/data.html}
"airports"
