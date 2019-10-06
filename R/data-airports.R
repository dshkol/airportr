#' Table of airport detail data
#'
#' A dataset containing names, codes, locations, altitude, and timezones for airports
#'
#' @format A data frame with 7698 rows and 14 variables:
#' \describe{
#'   \item{OpenFlights ID}{OpenFlights database ID}
#'   \item{Name}{Airport name, sometimes contains name of the city}
#'   \item{City}{Name of city served by airport}
#'   \item{IATA}{3-letter IATA code}
#'   \item{ICAO}{4-letter ICAO code}
#'   \item{Country}{Country name as in OpenFlights database. Note that country names may not be ISO 3166-1 standard.}
#'   \item{Country Code}{ISO 3166-1 numeric country code}
#'   \item{Country Code (Alpha-2)}{Name of city served by airport}
#'   \item{Country Code (Alpha-3)}{Name of country where airport is located}
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
