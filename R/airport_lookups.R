#' Return city name, airport name, IATA code, or IACO code given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code
#' @param input_type One of "name", "IATA", or "ICAO"
#' @param output_type One of "name", "city", "IATA", or "ICAO"
#' @return The appropriate city, airport name, IATA code, or ICAO code for that airport
#' @examples
#' add(1, 1)
#' add(10, 1)
airport_lookup <- function(input, input_type, output_type) {
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\",\"ICAO\", or \"name\"")}
  data("airports")
  if(input_type == output_type) {
    warning("Input type matches output type. Nothing to do.")
    }
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
    if(output_type == "name") {match %>% pull(Name) -> result}
    if(output_type == "ICAO") {match %>% pull(ICAO) -> result}
    if(output_type == "city") {match %>% pull(City) -> result}
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
    if(output_type == "name") {match %>% pull(Name) -> result}
    if(output_type == "IATA") {match %>% pull(IATA) -> result}
    if(output_type == "city") {match %>% pull(City) -> result}
  }
  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input)
    # Fix bug here
    if(length(match$Name) == 0) {
      similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
      if(length(similar)>0) {
      warning("No exact matches but some similar names in the database include:")
      cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.")}
    }
    if(output_type == "IATA") {match %>% pull(IATA) -> result}
    if(output_type == "ICAO") {match %>% pull(ICAO) -> result}
    if(output_type == "city") {match %>% pull(City) -> result}
  }
  return(result)
}

#' Return all airport details given an input IATA code, ICAO code, or airport name
#'
#' @param input An airport name, IATA code, or ICAO code
#' @param input_type One of "name", "IATA", or "ICAO"
#' @return A 1x14 tibble with airport details
#' @examples
#' add(1, 1)
#' add(10, 1)
airport_detail <- function(input, input_type) {
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\",\"ICAO\", or \"name\"")}
  data("airports")
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
  }
  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input)
    if(sum(lengths(match))==0) {
      similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
      if(length(similar)>0) {
        warning("No exact matches but some similar names in the database include:")
        cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.")
        }
    }
  }
  return(match)
}

#' Return all airports matching a city input
#'
#' @param city A city name
#' @param country (Optional) A country name
#' @return A nx14 tibble with airport details where n is the number of airports serving that city
#' @examples
#' add(1, 1)
#' add(10, 1)
city_airports <- function(city, country) {
  data("airports")
  if(missing(country)) {
    match<- airports %>% dplyr::filter(City == city)
    if(length(unique(match$Country)) > 1) {
      warning("City matches in cities in multiple countries. Do you want to specify a country   argument? See documentation ?city_airports for details. ")
    }
  } else {
    match <- airports %>% dplyr::filter(City == city, Country == country)
  }
  if(sum(lengths(match))==0) {
    similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
    if(length(similar)>0) {
      warning("No exact matches but some similarly named cities in the database include:")
      cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.")
      }
  }
}

#' Return airport location in lon/lat given an input IATA code, ICAO code, or airport name.
#'
#' @param input An airport name, IATA code, or ICAO code
#' @param input_type One of "name", "IATA", or "ICAO"
#' @return List of longitude and latitude coordinates
#' @examples
#' add(1, 1)
#' add(10, 1)
airport_location <- function(input, input_type) {
  if(!input_type %in% c("IATA","ICAO","name")) {
    stop("Input type must be one of \"IATA\",\"ICAO\", or \"name\"")}
  data("airports")
  if(input_type == "IATA") {
    match<- airports %>% dplyr::filter(IATA == input) %>% select(Latitude, Longitude)
    if(sum(lengths(match))==0) {stop("Unable to find matching IATA code.")}
  }
  if(input_type == "ICAO") {
    match<- airports %>% dplyr::filter(ICAO == input) %>% select(Latitude, Longitude)
    if(sum(lengths(match))==0) {stop("Unable to find matching ICAO code.")}
  }
  if(input_type == "name") {
    match<- airports %>% dplyr::filter(Name == input) %>% select(Latitude, Longitude)
    if(length(match$Latitude) == 0) {
      similar <- agrep(input, airports$Name, ignore.case = TRUE, value = TRUE)
      if(sum(lengths(match))==0) {
        warning("No exact matches but some similar names in the database include:")
        cat(similar, sep = "\n")} else {warning("Unable to find matching name in database.")
        }
    }
  }
  return(match)
}

